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
                   docker logout
                   '''
            }
        }
        stage ('Docs static site') {
            steps {
                sh 'nix build .#docs'
            }
        }
        stage ('Cachix push') {
            steps {
                cachixPush "nammayatri"
            }
        }
    }
}
