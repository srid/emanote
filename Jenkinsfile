pipeline {
    agent any
    stages {
        stage ('Hello') {
            steps {
                sh '''#!/run/current-system/sw/bin/bash
                    echo Hello'
                   '''
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