pipeline {
    agent any
    stages {
        stage ('Cachix setup') {
            steps {
                sh 'cachix use srid'
            }
        }
        stage ('Nix Build') {
            steps {
                sh 'nix build .#default'
            }
        }
    }
}