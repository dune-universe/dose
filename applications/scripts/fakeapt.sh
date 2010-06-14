#!/bin/bash
# Fri Jun 11 2010 Pietro Abate <pietro.abate@pps.jussieu.fr>

aptroot='/var/tmp/fakeapt'

TIMEOUT=30
PRODPID=

# define exit function
exit_timeout() {
  echo "Timeout. Checking for unfinished."
  for i in ${PRODPID} ; do
    ps -p $i |grep -v "PID TTY"
    if [ $? == 0 ] ; then
      # process still alive
      echo "Sending SIGTERM to process $i"
      kill $i
    fi
  done
  # timeout exit
  exit
}

#trap exit_timeout SIGUSR1
export PID=$$

fakeapt() {
    (apt-get -s \
        -o APT::Get::List-Cleanup="false" \
        -o Dir::Cache=$aptroot \
        -o Dir::State=$aptroot \
        -o Dir::State::status=$aptroot/status \
        -o Dir::Etc::SourceList=$aptroot/sources.list \
        -o APT::Install-Recommends="false" \
        -o APT::Architecture=amd64 \
        -o APT::Immediate-Configure="false" \
        $@) &
    PRODPID=$!
    (sleep $TIMEOUT ; kill -USR1 $PID) &
    TPID=$!
    wait ${PRODPID}
    kill $TPID 2>/dev/null
}

fakeaptitude() {
    (aptitude -s \
        -o APT::Get::List-Cleanup="false" \
        -o Dir::Cache=$aptroot \
        -o Dir::State=$aptroot \
        -o Dir::State::status=$aptroot/status \
        -o Dir::Etc::SourceList=$aptroot/sources.list \
        -o APT::Architecture=amd64 \
        -o APT::Install-Recommends="false" \
        -o APT::Immediate-Configure="false" \
        -o Aptitude::CmdLine::Fix-Broken="true" \
        -o Aptitude::CmdLine::Assume-Yes="true" \
        $@ ) &
    PRODPID=$!
    (sleep $TIMEOUT ; kill -USR1 $PID) &
    TPID=$!
    wait ${PRODPID}
    kill $TPID 2>/dev/null
}

initapt() {
  packages=$1
  status=$2
  echo $status
  echo $packages
  cp $status $aptroot/status
  cp $packages $aptroot/lists/Packages
cat<<EOF > $aptroot/sources.list
deb file:$aptroot/lists/ ./
EOF
fakeapt update
}

cleanup() {
  rm -Rf $aptroot
}

packages=$1
status=$2
shift 2
request=$@

mkdir -p $aptroot
mkdir -p $aptroot/lists
mkdir -p $aptroot/archives
mkdir -p $aptroot/archives/partial
mkdir -p $aptroot/lists/partial

initapt $packages $status
fakeapt $request
#cleanup
