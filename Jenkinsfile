@Library('GITDEMO_Shared_Lib@Git2IspwFts') _

String hostCredentialsId    = 'cwc2-16196'                    /* Change to Jenkins credentials ID for Host userid/password            */
String cesCredentialsId     = 'ces-0'                     /* Change to Jenkins credentials ID for CES token                       */
String gitRepoUrl           = 'https://github.com/vbugale/HDDRXM0_CWC2.git'   /* Change "<repo>" to user specific repository name                     */
String gitCredentialsId     = 'github-token'                     /* Change to Jenkins credentials ID for GitHub personal access token    */
String ccRepo               = 'SALESSUP.CWC2.GITA.COCO.REPOS'                     /* Change to user specific code coverage repository                     */

git2Ispw(
    executionEnvironment: 'cwc2',
    hostCredentialsId:    hostCredentialsId,
    cesCredentialsId:     cesCredentialsId, 
    gitCredentialsId:     gitCredentialsId, 
    gitRepoUrl:           gitRepoUrl,       
    ccRepo:               ccRepo
)
