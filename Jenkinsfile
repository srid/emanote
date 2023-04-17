pipeline {
    agent 'nixos'
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
