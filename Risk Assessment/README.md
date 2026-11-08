# README

This directory contains code used to assess disclosure risk of the synthetic data sets based on both Synthea and N3C. Code for each of these are kept in separate directories. Synthea itself is public, so should not have high disclosure risk in the source data, let alone the synthetic data, but it was used to test functionality. 

Risk assessment measures included in this package belong to two broad categories:

- Identity disclosure (re-identification): Identification of an individual by an intruder by exploring indirect identifiers. The following metrics are included:
    - UiS (% Unique in Synthetic): The percentage of records in the synthetic data that are unique with respect to select quasi-identifiers.
    - UiO (% Unique in Original): The percentage of records in the source data that are unique with respect to select quasi-identifiers.
    - RepU (Replicated Uniques): Among unique source records, % also unique in the synthetic data.
    - UiOiS (% Unique in Original in Synthetic): The percentage of records that are unique in the source that also appear in the synthetic data.

- Attribute disclosure: Sensitive values that may be inferred are attribute disclosure risks. If may be possible, for example, to analyze synthetic data to infer that all elderly mean in a small geographic area have dementia. The following metrics are included. 
    - Dorig (% Disclosive in Original): The percentage of source records that are disclosive, i.e, target attributes can be predicted from other attributes.
    - Dsyn (%Disclosive in Synthetic): The percentage of synthetic records that are disclosive.
    - DiSCO (Disclosive in the Synthetic and Correct): The proportion of the source records where quasi-identifiers in the synthetic data lead to correct prediction of attributes in the source data.
    - DiSDiO (Disclosive in Synthetic Disclosive in Original): A version of DiSCO limited to cases that are disclosive in the source data.
    - TCAP (Targeted Correct Attribution Probability): The probability that an adversary with an correctly infer a sensitive attribute for an individual known to be in the source data.
    - TCAP_1 (Targeted Correct Attribution Probability - Target 1): TCAP limited to health condition targets, with binary outcome 0/1, where value = 1, and 1 is assumed to be the sensitive outcome.
    - TCAP_0 (Targeted Correct Attribution Probability - Target 1): TCAP limited to health condition targets, with binary outcome 0/1, where value = 0.