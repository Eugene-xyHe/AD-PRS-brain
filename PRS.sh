# PRS

Rscript PRSice.R \
    --prsice PRSice_linux \
    --base Kunkle_etal_Stage1_results.txt \
    --target UKB_gene_v3_imp_imag_qc_chr# \
    --no-regress  \
    --binary-target T \
    --thread 20 \
    --snp MarkerName \
    --A1 Effect_allele \
    --A2 Non_Effect_allele \
    --chr Chromosome \
    --pvalue Pvalue \
    --bp Position \
    --stat Beta \
    --beta \
    --extract Kunkle_include_APOE.valid \
    --fastscore \
    --all-score \
    --bar-levels 0.00000005,0.0000005,0.000001,0.000005,0.00001,0.00005,0.0001,0.0005,0.001,0.005,0.01,0.05,0.1,0.2,0.3,0.4,0.5,1 \
    --out Kunkle_include_APOE


