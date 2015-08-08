BUILD=$(pwd)/build
DOWNLOADS=$BUILD/downloads

export PATH=$BUILD/bin:$PATH
export LANG=${LANG:-C.UTF-8}
export PREFIX=$BUILD

function run {
  OLD_PWD=$PWD
  cd $1
  shift
  echo RUNNING: $@
  $@
  if [ $? -ne 0 ]; then
    echo ============================
    echo = Aborting: Command failed =
    echo ============================

    exit 1
  fi
  cd $OLD_PWD
}

function cabal_install {
  cabal install --global --prefix=$BUILD --reorder-goals --max-backjumps=-1 $@
}
