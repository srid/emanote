pipeline {
    agent {
        label 'nixos'
    }
    stages {
        stage ('Platform Matrix') {
            matrix {
                agent {
                    label "${PLATFORM}"
                }
                axes {
                    axis {
                        name 'PLATFORM'
                        values 'nixos', 'macos'
                    }
                    axis {
                        name 'SYSTEM'
                        values 'x86_64-linux', 'aarch64-darwin', 'x86_64-darwin'
                    }
                }
                excludes {
                    exclude {
                        axis {
                            name 'PLATFORM'
                            value 'linux'
                        }
                        axis {
                            name 'SYSTEM'
                            notValues 'x86_64-linux'
                        }
                    }
                    exclude {
                        axis {
                            name 'PLATFORM'
                            value 'macos'
                        }
                        axis {
                            name 'SYSTEM'
                            values 'aarch64-darwin', 'x86_64-darwin'
                        }
                    }
                }
                stages {
                    stage ('Cachix setup') {
                        steps {
                            cachixUse 'srid'
                        }
                    }
                    stage ('Build') {
                        steps {
                            nixBuildAll system: env.SYSTEM
                        }
                    }
                    stage ('Cachix push') {
                        steps {
                            cachixPush "srid"
                        }
                    }
                }
            }
        }
    }
}