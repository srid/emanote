pipeline {
    agent any
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
                    stage ('Build') {
                        steps {
                            nixBuildAll ()
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