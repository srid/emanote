export default {
  import: ["step_definitions/**/*.ts", "support/**/*.ts"],
  paths: ["features/**/*.feature"],
  format: ["progress-bar", "pretty:/dev/stderr"],
  formatOptions: { snippetInterface: "async-await" },
};
