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
                // https://github.com/srid/nixci
                nixCI ()
            }
        }
        stage ('Cachix push') {
            steps {
                cachixPush "srid"
            }
        }
    }
}
