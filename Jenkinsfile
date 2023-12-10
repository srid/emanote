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
                // Ensure that closure size has not blown up.
                sh '''
                  MAX_CLOSURE_SIZE=$(echo "600 * 1000000" | nix run nixpkgs#bc)
                  CLOSURE_SIZE=$(nix path-info --json -S .#default | nix run nixpkgs#jq '.[0]'.closureSize)
                  echo "Emanote closure size: $CLOSURE_SIZE"
                  echo "    Max closure size: $MAX_CLOSURE_SIZE"
                  if [ "$CLOSURE_SIZE" -gt $MAX_CLOSURE_SIZE ]; then
                      echo "ERROR: Emanote's nix closure size has increased"
                      exit 3
                  fi
                '''
            }
        }
        stage ('Cachix push') {
            steps {
                cachixPush "srid"
            }
        }
    }
}
