from transforms.api import Pipeline

from myproject import datasets, synthetic_datasets

my_pipeline = Pipeline()
my_pipeline.discover_transforms(datasets)
my_pipeline.discover_transforms(synthetic_datasets)
