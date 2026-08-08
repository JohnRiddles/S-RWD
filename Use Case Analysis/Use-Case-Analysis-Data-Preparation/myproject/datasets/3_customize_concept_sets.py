from pyspark.sql import functions as F
from transforms.api import transform_df, Input, Output

output_path = (
    "/dummy_directory"
)
output_path += "/Utility-Assessment/Use-Case-Data/customize_concept_sets"


@transform_df(
    Output("ri.foundry.main.dataset.dummyid"),
    LL_concept_sets_fusion_everyone=Input(
        "ri.foundry.main.dataset.dummyid"
    ),
    LL_DO_NOT_DELETE_REQUIRED_concept_sets_all=Input(
        "ri.foundry.main.dataset.dummyid"
    ),
)
def customize_concept_sets(
    LL_concept_sets_fusion_everyone, LL_DO_NOT_DELETE_REQUIRED_concept_sets_all
):
    required = LL_DO_NOT_DELETE_REQUIRED_concept_sets_all
    customizable = LL_concept_sets_fusion_everyone

    df = required.join(customizable, on=required.columns, how="outer")

    return df
