pipeline {
    agent none
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
                        values 'x86_64-linux', 'aarch64-darwin'
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
                            nixBuildAll system: env.SYSTEM
                        }
                    }
                    stage ('Cachix push') {
                        steps {
                            cachixPush name: "srid"
                        }
                    }
                }
            }
        }
    }
}
