from pyspark.sql import functions as F
from transforms.api import transform_df, Input, Output

# Users: replace 'dummy_directory' references with appropriate directory paths

@transform_df(
    # specify the directory of existing data frame that was created in foundry
    output=Output(
        "/dummy_directory/copd_person_all_flags_s"
    ),
    person=Input(
        "/dummy_directory/copd_omop_person_ds"
    ),
    condition_occurrence=Input(
        "/dummy_directory/copd_omop_condition_occurrence_ds"
    ),
    drug_exposure=Input(
        "/dummy_directory/copd_omop_drug_exposure_ds"
    ),
    visit_occurrence=Input(
        "/dummy_directory/copd_omop_visit_occurrence_ds"
    ),
    procedure_occurrence=Input(
        "/dummy_directory/copd_omop_procedure_occurrence_ds"
    ),
    copd_patient_deaths=Input(
        "/dummy_directory/copd_patient_deaths_s"
    ),
    copd_person_obesity=Input(
        "/dummy_directory/copd_person_obesity_s"
    ),
    concept_set_members=Input(
        "/dummy_directory/concept_set_members"
    ),
    customize_concept_sets=Input(
        "/dummy_directory/customized_concept_sets_dataset"
    ),
)

# Description - This node filters the condition_eras table for rows that have a condition_concept_id associated with one of the concept sets described in the data dictionary in the README through the use of a fusion sheet.  Indicator names for these conditions are assigned, and the indicators are collapsed to unique instances on the basis of patient and date.


def copd_person_all_flags(
    person,
    condition_occurrence,
    drug_exposure,
    visit_occurrence,
    procedure_occurrence,
    copd_patient_deaths,
    copd_person_obesity,
    concept_set_members,
    customize_concept_sets,
):

    # bring in only cohort patient ids
    persons = person.select("person_id")

    # filter observations table to only cohort patients
    conditions_df = (
        condition_occurrence.select(
            "person_id", "condition_start_date", "condition_concept_id"
        )
        .where(F.col("condition_start_date").isNotNull())
        .withColumnRenamed("condition_start_date", "date")
        .withColumnRenamed("condition_concept_id", "concept_id")
        .join(persons, "person_id", "inner")
    )
    drug_df = (
        drug_exposure.select("person_id", "drug_exposure_start_date", "drug_concept_id")
        .where(F.col("drug_exposure_start_date").isNotNull())
        .withColumnRenamed("drug_exposure_start_date", "date")
        .withColumnRenamed("drug_concept_id", "concept_id")
        .join(persons, "person_id", "inner")
    )
    visit_df = (
        visit_occurrence.select("person_id", "visit_start_date", "visit_concept_id")
        .where(F.col("visit_start_date").isNotNull())
        .withColumnRenamed("visit_start_date", "date")
        .withColumnRenamed("visit_concept_id", "concept_id")
        .join(persons, "person_id", "inner")
    )
    procedure_df = (
        procedure_occurrence.select(
            "person_id", "procedure_date", "procedure_concept_id"
        )
        .where(F.col("procedure_date").isNotNull())
        .withColumnRenamed("procedure_date", "date")
        .withColumnRenamed("procedure_concept_id", "concept_id")
        .join(persons, "person_id", "inner")
    )

    death_df = copd_patient_deaths.select(
        "person_id", "patient_death"
    ).withColumnRenamed("patient_death", "DEATH")

    # filter fusion sheet for concept sets and their future variable names that have concepts in the conditions domain
    # filter to only concept ids for the conditions of interest
    concepts_df = customize_concept_sets.filter(
        customize_concept_sets.Table.contains("CO")
    ).select("concept_id", "indicator_suffix")
    # find conditions information based on matching concept ids for conditions of interest
    c_df = conditions_df.join(concepts_df, "concept_id", "inner")
    # collapse to unique person and visit date and pivot on future variable name to create flag for rows associated with the concept sets for conditions of interest
    c_df = c_df.groupby("person_id").pivot("indicator_suffix").agg(F.lit(1)).na.fill(0)

    concepts_df = customize_concept_sets.filter(
        customize_concept_sets.Table.contains("DE")
    ).select("concept_id", "indicator_suffix")
    # find conditions information based on matching concept ids for conditions of interest
    d_df = drug_df.join(concepts_df, "concept_id", "inner")
    # collapse to unique person and visit date and pivot on future variable name to create flag for rows associated with the concept sets for conditions of interest
    d_df = d_df.groupby("person_id").pivot("indicator_suffix").agg(F.lit(1)).na.fill(0)

    concepts_df = customize_concept_sets.filter(
        customize_concept_sets.Table.contains("PO")
    ).select("concept_id", "indicator_suffix")
    # find conditions information based on matching concept ids for conditions of interest
    p_df = procedure_df.join(concepts_df, "concept_id", "inner")
    # collapse to unique person and visit date and pivot on future variable name to create flag for rows associated with the concept sets for conditions of interest
    p_df = p_df.groupby("person_id").pivot("indicator_suffix").agg(F.lit(1)).na.fill(0)

    concepts_df = customize_concept_sets.filter(
        customize_concept_sets.Table.contains("VO")
    ).select("concept_id", "indicator_suffix")
    # find conditions information based on matching concept ids for conditions of interest
    v_df = visit_df.join(concepts_df, "concept_id", "inner")
    # collapse to unique person and visit date and pivot on future variable name to create flag for rows associated with the concept sets for conditions of interest
    v_df = v_df.groupby("person_id").pivot("indicator_suffix").agg(F.lit(1)).na.fill(0)

    df = (
        persons.join(c_df, "person_id", "left")
        .join(d_df, "person_id", "left")
        .join(p_df, "person_id", "left")
        .join(v_df, "person_id", "left")
        .join(death_df, "person_id", "left")
        .join(copd_person_obesity, "person_id", "left")
        .na.fill(0)
    )

    return df
