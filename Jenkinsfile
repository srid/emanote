pipeline {
    agent any
    environment {
      CACHIX_AUTH_TOKEN = credentials('cachix-auth-token')
    }
    stages {
        stage ('Cachix setup') {
            steps {
                sh 'cachix use srid'
            }
        }
        stage ('Build') {
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
        stage ('Push to cachix') {
          steps {
            sh 'nix run .#cachix-push'
          }
        }
    }
}
