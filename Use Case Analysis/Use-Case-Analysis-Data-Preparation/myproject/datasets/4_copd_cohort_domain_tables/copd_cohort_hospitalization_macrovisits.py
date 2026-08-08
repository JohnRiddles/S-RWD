from transforms.api import Input, Output, transform_df
from pyspark.sql import functions as F
from pyspark.sql.window import Window

output_path = "/dummy_directory"
output_path += "/Utility-Assessment/Use-Case-Data/copd_cohort_hospitalization_macrovisits"


@transform_df(
    Output(output_path),
    visit_occurrence=Input("ri.foundry.main.dataset.dummyid"),
)
def copd_cohort_hospitalization_macrovisits(visit_occurrence):
    HOSPITALIZATION_CONCEPTS = [262, 8717, 9201, 581379, 32037]

    # Step 1: Filter to hospitalization visit types with valid date ranges
    scaffold = visit_occurrence.filter(
        F.col("visit_concept_id").isin(HOSPITALIZATION_CONCEPTS)
        & F.col("visit_start_date").isNotNull()
        & F.col("visit_end_date").isNotNull()
        & (F.col("visit_end_date") >= F.col("visit_start_date"))
    ).select("person_id", "visit_occurrence_id", "visit_concept_id", "visit_start_date", "visit_end_date")

    # Step 2: Running max of visit_end_date over all PRECEDING rows (excludes current row),
    # partitioned by person_id and ordered by visit_start_date.
    # This mirrors Polars' cum_max().shift(1).over("person_id").
    preceding_window = (
        Window.partitionBy("person_id").orderBy("visit_start_date").rowsBetween(Window.unboundedPreceding, -1)
    )

    scaffold = scaffold.withColumn("running_max_end_date", F.max("visit_end_date").over(preceding_window))

    # Step 3: Flag the start of a new macrovisit when there is a gap of >= 1 day
    # from the running max end date, or when running_max_end_date is null (first row per person).
    scaffold = scaffold.withColumn(
        "new_macrovisit_flag",
        F.when(
            F.col("running_max_end_date").isNull()
            | (F.datediff(F.col("visit_start_date"), F.col("running_max_end_date")) >= 1),
            1,
        ).otherwise(0),
    )

    # Step 4: Cumulative sum of the flag gives a unique group ID per person.
    # This mirrors Polars' cum_sum().over("person_id").
    cumsum_window = (
        Window.partitionBy("person_id").orderBy("visit_start_date").rowsBetween(Window.unboundedPreceding, 0)
    )

    scaffold = scaffold.withColumn("within_person_macrovisit_id", F.sum("new_macrovisit_flag").over(cumsum_window))

    # Step 5: Build the macrovisit_id and select final columns (keep dates for Step 6)
    hospitalization_macrovisits = scaffold.withColumn(
        "macrovisit_id",
        F.concat(F.col("person_id").cast("string"), F.lit("_"), F.col("within_person_macrovisit_id").cast("string")),
    ).select(
        "person_id",
        "macrovisit_id",
        "visit_occurrence_id",
        "visit_start_date",
        "visit_end_date",  # retained for Step 6
    )

    # Step 6: Compute macrovisit-level start and end dates
    macrovisit_window = Window.partitionBy("macrovisit_id")

    hospitalization_macrovisits = (
        hospitalization_macrovisits.withColumn(
            "macrovisit_start_date", F.min("visit_start_date").over(macrovisit_window)
        )
        .withColumn("macrovisit_end_date", F.max("visit_end_date").over(macrovisit_window))
        .select("person_id", "macrovisit_id", "visit_occurrence_id", "macrovisit_start_date", "macrovisit_end_date")
    )

    # Step 7: Group macrovisits into combined macrovisits if the start date of a macrovisit
    # is within 30 days of the previous macrovisit's start date (per person).

    # Work on distinct macrovisits to avoid duplicating logic across visit_occurrence rows.
    macrovisit_window = Window.partitionBy("person_id").orderBy("macrovisit_start_date")

    distinct_macrovisits = hospitalization_macrovisits.select(
        "person_id", "macrovisit_id", "macrovisit_start_date"
    ).distinct()

    distinct_macrovisits = distinct_macrovisits.withColumn(
        "prev_macrovisit_start_date", F.lag("macrovisit_start_date", 1).over(macrovisit_window)
    )

    distinct_macrovisits = distinct_macrovisits.withColumn(
        "new_combined_flag",
        F.when(
            F.col("prev_macrovisit_start_date").isNull()
            | (F.datediff(F.col("macrovisit_start_date"), F.col("prev_macrovisit_start_date")) > 30),
            1,
        ).otherwise(0),
    )

    cumsum_combined_window = (
        Window.partitionBy("person_id").orderBy("macrovisit_start_date").rowsBetween(Window.unboundedPreceding, 0)
    )

    distinct_macrovisits = (
        distinct_macrovisits.withColumn(
            "within_person_combined_id", F.sum("new_combined_flag").over(cumsum_combined_window)
        )
        .withColumn(
            "combined_macrovisit_id",
            F.concat(F.col("person_id").cast("string"), F.lit("_"), F.col("within_person_combined_id").cast("string")),
        )
        .select("macrovisit_id", "combined_macrovisit_id")
    )

    # Join combined_macrovisit_id back onto the main output
    hospitalization_macrovisits = hospitalization_macrovisits.join(
        distinct_macrovisits, on="macrovisit_id", how="left"
    ).select(
        "person_id",
        "combined_macrovisit_id",
        "macrovisit_id",
        "visit_occurrence_id",
        "macrovisit_start_date",
        "macrovisit_end_date",
    )

    # Step 8: Compute combined macrovisit start and end dates
    combined_macrovisit_window = Window.partitionBy("combined_macrovisit_id")

    hospitalization_macrovisits = (
        hospitalization_macrovisits.withColumn(
            "combined_macrovisit_start_date", F.min("macrovisit_start_date").over(combined_macrovisit_window)
        )
        .withColumn("combined_macrovisit_end_date", F.max("macrovisit_end_date").over(combined_macrovisit_window))
        .select(
            "person_id",
            "combined_macrovisit_id",
            "macrovisit_id",
            "visit_occurrence_id",
            "combined_macrovisit_start_date",
            "combined_macrovisit_end_date",
            "macrovisit_start_date",
            "macrovisit_end_date",
        )
    )

    return hospitalization_macrovisits
