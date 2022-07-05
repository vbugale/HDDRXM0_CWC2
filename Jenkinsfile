node {
  stage ('Checkout') 
  {
    // Get the code from the Git repository
    checkout scm
  }
  
  stage('Git to ISPW Synchronization')
  { 
    gitToIspwIntegration app: 'GITA', 
    branchMapping: '''ispw_demo => FT1, per-commit
      ''',
    connectionId: 'cwc2-16196', 
    credentialsId: 'cwc2-cwezxx2', 
    gitCredentialsId: 'bitbucket', 
    gitRepoUrl: 'https://bitbucket.zeng.bmc.com/scm/~vbugale/gitplay.git', 
    runtimeConfig: 'isp8', 
    stream: 'GITDEMO1',
    ispwConfigPath: './GenAppCore/ispwconfig.yml'
  }  
}
