# NOT CURRENTLY CALLED WITHIN MAIN PIPELINE but serves as a starting point for
# users wishing to isolate parameters in Slurm environments like Frontier

import os

# =========================================================
# 1. Base Environment Path
# =========================================================
BASE_SCRATCH_DIR = ""

# =========================================================
# 2. Sub-Directory Structures
# =========================================================
_relative_dirs = [
    "omop_synthea",
    "omop_synthea_sample",
    "omop_synthea_sample/cehrgpt",
    "omop_synthea_sample/dataset_prepared",
    "omop_synthea_sample/cehrgpt/synthetic_data"
]

# Prepend the BASE_SCRATCH_DIR to all of the directories
DIRECTORIES_TO_CREATE = [os.path.join(BASE_SCRATCH_DIR, d) for d in _relative_dirs]
# =========================================================
# 3. Tar Extraction & Validation
# =========================================================
TAR_SOURCE_FILE = "omop_synthea.tar"
TAR_DEST_DIR = os.path.join(BASE_SCRATCH_DIR, "omop_synthea")
PANDAS_PERSON_PATH = os.path.join(BASE_SCRATCH_DIR, "omop_synthea", "person")

# =========================================================
# 4. PySpark Environment Configuration
# =========================================================
SPARK_ENV_VARS = {
    "SPARK_WORKER_INSTANCES": "1",
    "SPARK_WORKER_CORES": "16",
    "SPARK_EXECUTOR_CORES": "8",
    "SPARK_DRIVER_MEMORY": "20g",
    "SPARK_EXECUTOR_MEMORY": "20g",
    "SPARK_MASTER": "local[16]"
}

# =========================================================
# 5. OMOP Sampling Paths
# =========================================================
# Person table sampling
PERSON_SRC_PATH = os.path.join(BASE_SCRATCH_DIR, "omop_synthea", "person")
OMOP_PERSON_SAMPLE_PATH = os.path.join(BASE_SCRATCH_DIR, "omop_synthea_sample", "person")
# All OMOP folders
OMOP_FOLDER_PATH = os.path.join(BASE_SCRATCH_DIR, "omop_synthea")
OMOP_OUTPUT_FOLDER_PATH = os.path.join(BASE_SCRATCH_DIR, "omop_synthea_sample")
OMOP_DATA_FOLDER_PATH = os.path.join(BASE_SCRATCH_DIR, "omop_synthea_sample", "dataset_prepared")
OMOP_CEHRGPT_FOLDER_PATH = os.path.join(BASE_SCRATCH_DIR, "omop_synthea_sample", "cehrgpt")
OMOP_SYNTHETIC_DATA_FOLDER_PATH = os.path.join(BASE_SCRATCH_DIR, "omop_synthea_sample", "cehrgpt", "synthetic_data")
# =========================================================
# 6. Vocabulary Data Paths
# =========================================================
OMOP_DATA_DIR = os.path.join(BASE_SCRATCH_DIR, "omop_data")
VOCAB_TABLES = [
    "concept", 
    "concept_ancestor", 
    "concept_relationship"
]

# =========================================================
# 7. Concept List Generation Parameters
# =========================================================
MIN_NUM_OF_PATIENTS = '100'  # Adjust to 10 or 1000 for testing
EHR_TABLE_LIST = [
    "condition_occurrence",
    "procedure_occurrence",
    "drug_exposure"
    # "measurement",
    # "observation",
]

# =========================================================
# 8. Training Data Generation Parameters
# =========================================================
TRAINING_START_DATE = '1985-01-01'
ATT_TYPE = 'day'
INPATIENT_ATT_TYPE = 'day'
DOMAIN_TABLE_LIST = [
    "condition_occurrence",
    "drug_exposure",
    "procedure_occurrence"
    # "measurement",
    # "observation",
]

# =========================================================
# 9. SLURM Cluster & Cache Configuration
# =========================================================
SLURM_MAIL_USER = ""
CONDA_BASH_HOOK_PATH = ""
HF_HOME_DIR = os.path.join(BASE_SCRATCH_DIR, ".cache")

# =========================================================
# 10. Model Training Hyperparameters
# =========================================================
TRAIN_SLURM_JOB_NAME = "cehrgpt_train_slurm_1mil"
TRAIN_SLURM_TIME = "6:00:00"
TRAIN_SLURM_CPUS = 32
TRAIN_SLURM_GPUS = 8
TRAIN_SLURM_PARTITION = "extended"

DATALOADER_NUM_WORKERS = 16
DATALOADER_PREFETCH_FACTOR = 8
TRAIN_HIDDEN_SIZE = 768
TRAIN_NUM_LAYERS = 12
TRAIN_MAX_POS_EMBEDDINGS = 1024
MAX_TOKENS_PER_BATCH = 4096
WARMUP_RATIO = 0.01
WEIGHT_DECAY = 0.01
NUM_TRAIN_EPOCHS = 10
LEARNING_RATE = 0.0002
EARLY_STOPPING_THRESHOLD = 0.001

# =========================================================
# 11. Synthetic Sequence Generation Hyperparameters
# =========================================================
GENERATE_SLURM_JOB_NAME = "cehrgpt_seq_gen_100k"
GENERATE_BATCH_SIZE = 64
GENERATE_BUFFER_SIZE = 1024
GENERATE_CONTEXT_WINDOW = 1024
GENERATE_SAMPLING_STRATEGY = "TopPStrategy"
GENERATE_TOP_P = 0.95
GENERATE_TEMPERATURE = 0.9
GENERATE_REPETITION_PENALTY = 1.05
GENERATE_EPSILON_CUTOFF = 0.00

# =========================================================
# 12. OMOP Format Conversion Parameters
# =========================================================
CONVERT_PATIENT_SEQUENCE_PATH = os.path.join(BASE_SCRATCH_DIR, "omop_synthea_sample", "stacked_output") #folder where sequences are stored
CONVERT_OUTPUT_FOLDER = os.path.join(BASE_SCRATCH_DIR, "omop_synthea_sample", "restored_omop") #output folder where the restored OMOP tables will be stored
CONVERT_CONCEPT_PATH = os.path.join(BASE_SCRATCH_DIR, "omop_synthea_sample", "concept") #folder for concept tables
CONVERT_CPU_CORES = 8
CONVERT_BUFFER_SIZE = 1024

# =========================================================
# 13. Post-Processing Paths (For the Randomization Script)
# =========================================================
RESTORED_PERSON_PATH = os.path.join(CONVERT_OUTPUT_FOLDER, "person")
RESTORED_VISIT_OCCURRENCE_PATH = os.path.join(CONVERT_OUTPUT_FOLDER, "visit_occurrence")