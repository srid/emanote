pipeline {
    agent {
        label 'nixos'
    }
    stages {
        stage ('Platform Matrix') {
            matrix {
                agent {
                    label "${PLATFORM}"
                }
                axes {
                    axis {
                        name 'PLATFORM'
                        values 'nixos', 'macos'
                    }
                }
                stages {
                    stage ('Cachix setup') {
                        steps {
                            cachixUse 'srid'
                        }
                    }
                    stage ('Build ${PLATFORM}') {
                        steps {
                            nixBuildAll ()
                        }
                    }
                    stage ('Rosetta Build') {
                        when {
                            expression {
                                "${PLATFORM}" == "macos"
                            }
                        }
                        steps {
                            nixBuildAll system: "x86_64-darwin"
                        }
                    }
                    stage ('Cachix push') {
                        steps {
                            cachixPush "srid"
                        }
                    }
                }
            }
        }
    }
}