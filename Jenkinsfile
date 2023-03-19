pipeline {
    agent any
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
            environment {
              DOCKER_PASS = credentials('docker-pass')
            }
            steps {
                sh 'docker load -i $(nix build .#dockerImage --print-out-paths)'
                sh '''
                   echo ${DOCKER_PASS} | docker login -u sridca --password-stdin
                   # TODO: waiting on porting gh-pages deployment here
                   # docker push sridca/emanote:latest
                   '''
            }
        }
        stage ('Docs static site') {
            steps {
                sh 'nix build .#docs'
            }
        }
        stage ('Push to cachix') {
          environment {
            CACHIX_AUTH_TOKEN = credentials('cachix-auth-token')
          }
          steps {
            sh 'nix run .#cachix-push'
          }
        }
    }
}
