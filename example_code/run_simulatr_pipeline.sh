source ~/.research_config

nextflow $LOCAL_CODE_DIR"simulatr-pipeline/main.nf" \
  --simulatr_specifier_fp $HOME"/simulatr_dir/sim_spec_obj.rds" \
  --B 10 \
  --result_dir $HOME"/simulatr_dir" \
  --result_file_name "result.rds" \
  -resume
