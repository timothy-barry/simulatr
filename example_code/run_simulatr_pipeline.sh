source ~/.research_config

nextflow pull katsevich-lab/simulatr-pipeline
nextflow run katsevich-lab/simulatr-pipeline \
  --simulatr_specifier_fp $HOME"/simulatr_dir/sim_spec_obj.rds" \
  --B 10 \
  --result_dir $HOME"/simulatr_dir" \
  --result_file_name "result.rds"
