pipeline {
    agent any
    stages {
        stage ('Hello') {
            steps {
                sh 'echo Hello'
            }
        }
        stage ('Cachix setup') {
            steps {
                sh 'nix run nixpkgs#cachix use srid'
            }
        }
        stage ('Nix Build') {
            steps {
                sh 'nix build .#default'
            }
        }
    }
}