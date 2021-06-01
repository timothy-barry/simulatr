// define pipline input parameters
params.simulatr_obj = "~/research_code/simulatr-project/ex_sim_obj.rds"
params.meta_params = "metaparams.txt"

// put the metaparams into a map
meta_params = [:]
my_file = file(params.meta_params)
all_lines = my_file.readLines()
for (line : all_lines) {
    str_split = line.split(':')
    key = str_split[0]
    if (key == "method_names") {
      value = str_split[1].split('-')
    } else {
      value = str_split[1]
    }
    meta_params[key] = value
}

/*************
Generate data
*************/
n_param_settings = meta_params["n_param_settings"].toInteger()
param_idx_ch = Channel.of(1..n_param_settings)
process generate_data {
  echo true

  input:
  val i from param_idx_ch

  // output:
  // file 'data_list.rds' into data_ch

  """
  echo $i
  """

  /*
  """
  Rscript $PWD/Rscripts/generate_data.R $params.simulatr_obj $i data_list.rds
  """
  */
}
