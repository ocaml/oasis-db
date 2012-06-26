#!/bin/bash 

set -e 
set -x

CI_URL="http://mini:8080/job/oasis-db/lastSuccessfulBuild/artifact/dist"
DEPLOY_HOST=ssh.ocamlcore.org
DEPLOY_DIR_BASE=/home/groups/oasis/ocsigen
DEPLOY_DIR=$DEPLOY_DIR_BASE/dev-$(oasis query version)-$(date +%F)
DEPLOY_TMP=_build/deploy-tmp-$RANDOM
SSH_MASTER_FN=deploy-dev-master-$RANDOM.ssh
SSH_MASTER_PID=

clean_exit () {
  rm -rf $SSH_MASTER_FN $DEPLOY_TMP
}

trap "clean_exit" EXIT

remote_cmd () {
  ssh -S $SSH_MASTER_FN $DEPLOY_HOST "$@"
}

mkdir -p $DEPLOY_TMP
curl -o $DEPLOY_TMP/deploy-dev.zip $CI_URL/deploy-dev.zip
curl -o $DEPLOY_TMP/tag.darcs $CI_URL/tag.darcs
pushd $DEPLOY_TMP 
unzip deploy-dev.zip
popd 

ssh -M -S $SSH_MASTER_FN $DEPLOY_HOST 'read' &
SSH_MASTER_PID=$!

# Deploy to a new directory
chmod -R o-w $DEPLOY_TMP/deploy-dev/
rsync -e "ssh -t -S $SSH_MASTER_FN" -av $DEPLOY_TMP/deploy-dev/* $DEPLOY_HOST:$DEPLOY_DIR

# Transfer data 
remote_cmd "cd $DEPLOY_DIR_BASE/dev && ./daemon.sh stop"
remote_cmd "cp -a $DEPLOY_DIR_BASE/dev/data $DEPLOY_DIR/"
remote_cmd "cp -a $DEPLOY_DIR_BASE/dev/etc/ocamlcore-api.ini $DEPLOY_DIR/etc/"
remote_cmd "rm $DEPLOY_DIR_BASE/dev"
remote_cmd "ln -s $DEPLOY_DIR $DEPLOY_DIR_BASE/dev"
remote_cmd "cd $DEPLOY_DIR_BASE/dev && ./daemon.sh start"

#darcs apply $DEPLOY_TMP/tag.darcs
