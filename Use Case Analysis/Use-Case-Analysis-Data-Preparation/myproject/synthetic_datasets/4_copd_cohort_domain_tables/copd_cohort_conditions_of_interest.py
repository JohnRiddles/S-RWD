from pyspark.sql import functions as F
from transforms.api import transform_df, Input, Output

output_path = "/dummy_directory"
output_path += "/Utility-Assessment/Use-Case-Data/Synthetic-Use-Case-Data/copd_cohort_conditions_of_interest"

copd_cohort_de_id_path = "/dummy_directory"
copd_cohort_de_id_path += "/Utility-Assessment/Use-Case-Data/Synthetic-Use-Case-Data/copd_cohort_de_id"

@transform_df(
    Output(output_path),
    copd_cohort_de_id=Input(copd_cohort_de_id_path),
    concept_set_members=Input("ri.foundry.main.dataset.dummyid"),
    condition_occurrence=Input("ri.foundry.main.dataset.dummyid"),
    customize_concept_sets=Input("ri.foundry.main.dataset.dummyid"),
    additional_custom_concept_sets=Input("ri.foundry.main.dataset.dummyid"),
)

# copd_cohort_conditions_of_interest
# Purpose - The purpose of this pipeline is to produce a day level and a persons level fact table for all patients in the N3C enclave.
# Description - This node filters the condition_eras table for rows that have
# a condition_concept_id associated with one of the concept sets
# described in the data dictionary in the README through the use of a fusion sheet.
# Indicator names for these conditions are assigned,
# and the indicators are collapsed to unique instances on the basis of patient and date.


def copd_cohort_conditions_of_interest(
    copd_cohort_de_id, concept_set_members, condition_occurrence, customize_concept_sets, additional_custom_concept_sets
):
    # bring in only cohort patient ids
    persons = copd_cohort_de_id.select("person_id")
    # filter condition_occurrence table to only cohort patients
    conditions_df = (
        condition_occurrence.select("person_id", "condition_start_date", "condition_concept_id")
        .where(F.col("condition_start_date").isNotNull())
        .withColumnRenamed("condition_start_date", "date")
        .withColumnRenamed("condition_concept_id", "concept_id")
        .join(persons, "person_id", "inner")
    )

    # filter fusion sheet for concept sets and their future variable names that have concepts in the conditions domain
    fusion_df = customize_concept_sets.filter(customize_concept_sets.domain.contains("condition")).select(
        "concept_set_name", "indicator_prefix"
    )
    # filter concept set members table to only concept ids for the conditions of interest
    concepts_df = (
        concept_set_members.select("concept_set_name", "is_most_recent_version", "concept_id")
        .where(F.col("is_most_recent_version") == "true")
        .join(fusion_df, "concept_set_name", "inner")
        .select("concept_id", "indicator_prefix")
    )
    # add additional custom concept sets
    concepts_df = concepts_df.unionByName(additional_custom_concept_sets.select("concept_id", "indicator_prefix"))

    # find conditions information based on matching concept ids for conditions of interest
    df = conditions_df.join(concepts_df, "concept_id", "inner")
    # collapse to unique person and visit date and pivot on future variable name to create flag for rows associated with the concept sets for conditions of interest
    df = df.groupby("person_id", "date").pivot("indicator_prefix").agg(F.lit(1)).na.fill(0)

    return df
