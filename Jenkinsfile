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
    gitCredentialsId: 'github-token', 
    gitRepoUrl: 'https://github.com/vbugale/HDDRXM0_CWC2.git', 
    runtimeConfig: 'isp8', 
    stream: 'CWEZ',
    ispwConfigPath: './GenAppCore/ispwconfig.yml'
  }  
}
