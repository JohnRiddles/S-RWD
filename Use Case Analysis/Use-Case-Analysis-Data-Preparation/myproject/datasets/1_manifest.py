from typing import Any
from transforms.api import transform_df, Input, Output
from pyspark.sql import functions as F


@transform_df(
    Output("ri.foundry.main.dataset.dummyid"),
    manifest_fusion=Input(
        "ri.foundry.main.dataset.dummyid"
    ),
)
def manifest(manifest_fusion):
    df: Any = manifest_fusion
    # df = df.withColumn(F.to_date(df.run_date).alias('run_date'))
    df = (
        df.withColumn("run_date", F.to_date(df.RunDate))
        .withColumn("contribution_date", F.to_date(df.ContributionDate))
        .withColumn("data_partner_id", F.col("data_partner_id").cast("integer"))
    )

    return df
