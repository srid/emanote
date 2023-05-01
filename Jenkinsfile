pipeline {
    agent any
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
        stage ('Build, macOS') {
            steps {
                sh 'nix --option system aarch64-darwin -j0 build -L'
            }
        }
        stage ('Cachix push') {
            steps {
                cachixPush "srid"
            }
        }
    }
}
