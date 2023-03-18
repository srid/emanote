pipeline {
    agent any
    stages {
        stage ('Cachix setup') {
            steps {
                sh 'cachix use srid'
            }
        }
        stage ('Haskell package') {
            steps {
                sh 'nix build .#default'
            }
        }
        stage ('Docker image') {
            steps {
                sh 'nix build .#dockerImage'
            }
        }
        stage ('Docs static site') {
            steps {
                sh 'nix build .#docs'
            }
        }
    }
}
