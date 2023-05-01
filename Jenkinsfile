pipeline {
    agent any
    stages {
        stage ('Cachix setup') {
            steps {
                cachixUse 'srid'
            }
        }
        stage ('Nix Build') {
            steps {
                parallel(
                    native: {
                        nixBuildAll ()
                    },
                    macOS: {
                        sh 'nix --option system aarch64-darwin -j0 build -L'
                    },
                    macOS_Intel: {
                        sh 'nix --option system x86_64-darwin -j0 build -L'
                    }
                )
            }
        }
        stage ('Cachix push') {
            steps {
                cachixPush "srid"
            }
        }
    }
}
