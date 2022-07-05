node {
  stage ('Checkout') 
  {
    // Get the code from the Git repository
    checkout scm
  }
  
  stage('Git to ISPW Synchronization')
  { 
    gitToIspwIntegration app: 'TXX2', 
    branchMapping: '''ispw_demo => DEV1, per-commit
      ''',
    connectionId: 'cwc2-16196', 
    credentialsId: 'cwc2-cwezxx2', 
    gitCredentialsId: 'bitbucket', 
    gitRepoUrl: 'https://bitbucket.zeng.bmc.com/scm/~vbugale/gitplay.git', 
    runtimeConfig: 'isp8', 
    stream: 'CWEZ'
  }  
}
