pipeline {
    agent {
        label 'nixos'
    }
    stages {
        stage ('OS Matrix') {
            matrix {
                agent {
                    label "${OS}"
                }
                axes {
                    axis {
                        name 'OS'
                        values 'nixos', 'macos'
                    }
                    axis {
                        name 'SYSTEM'
                        values 'x86_64-linux', 'aarch64-darwin', 'x86_64-darwin'
                    }
                }
                excludes {
                    exclude {
                        axis {
                            name 'OS'
                            values 'nixos'
                        }
                        axis {
                            name 'SYSTEM'
                            notValues 'x86_64-linux'
                        }
                    }
                    exclude {
                        axis {
                            name 'OS'
                            values 'macos'
                        }
                        axis {
                            name 'SYSTEM'
                            notValues 'aarch64-darwin', 'x86_64-darwin'
                        }
                    }
                }
                stages {
                    stage ('Cachix setup') {
                        steps {
                            cachixUse 'srid'
                        }
                    }
                    stage ('Build') {
                        steps {
                            script {
                                flakeOutputs = nixBuildAll system: env.SYSTEM
                                env.FLAKE_OUTPUTS = flakeOutputs.trim()
                            }
                        }
                    }
                    stage ('Cachix push') {
                        steps {
                            cachixPush name: "srid", paths: env.FLAKE_OUTPUTS
                        }
                    }
                }
            }
        }
    }
}