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
        stage ('Docker Build') {
            steps {
                sh 'nix build -j auto .#dockerImage'
            }
        }
        stage ('Docs Build') {
            steps {
                sh 'nix build .#docs'
            }
        }
    }
}
