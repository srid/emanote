pipeline {
    agent any
    stages {
        stage ('Cachix setup') {
            steps {
                cachixUse 'srid'
            }
        }
        stage ('Nix Build') {
                parallel{
                    stage('Linux'){
                        agent {
                            label "nixos"
                        }
                        steps {
                            nixBuildAll ()
                        }
                    }
                    stage('macOS'){
                        agent {
                            label "macos"
                        }
                        steps {
                            nixBuildAll ()
                        }
                    }
                }
            }
        }
        stage ('Cachix push') {
            steps {
                cachixPush "srid"
            }
        }
    }
}
