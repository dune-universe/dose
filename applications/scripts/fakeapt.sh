#!/bin/bash
# Fri Jun 11 2010 Pietro Abate <pietro.abate@pps.jussieu.fr>

set -x 
aptroot='/var/tmp/fakeapt'

fakeapt() {
  action=$1
  apt-get -s \
      -o APT::Get::List-Cleanup="false" \
      -o Dir::Cache=$aptroot \
      -o Dir::State=$aptroot \
      -o Dir::State::status=$aptroot/status \
      -o Dir::Etc::SourceList=$aptroot/sources.list \
      -o APT::Install-Recommends="false" \
      -o APT::Architecture="amd64" \
      -o APT::Immediate-Configure="false" \
      $action `cat Request`
}

      #-o Aptitude::CmdLine::Fix-Broken="true" \

fakeaptitude() {
  action=$1
  echo "\n" | aptitude -s \
      -o APT::Get::List-Cleanup="false" \
      -o Dir::Cache=$aptroot \
      -o Dir::State=$aptroot \
      -o Dir::State::status=$aptroot/status \
      -o Dir::Etc::SourceList=$aptroot/sources.list \
      -o APT::Architecture="amd64" \
      -o APT::Install-Recommends="false" \
      -o APT::Immediate-Configure="false" \
      -o Aptitude::CmdLine::Assume-Yes="true" \
      -o Aptitude::Auto-Fix-Broken="true" \
      -o Aptitude::ProblemResolver::Discard-Null-Solution="false" \
      --allow-untrusted -v -y --full-resolver $action `cat Request`
}

fixfakeaptitude() {
  aptitude -s \
      -o APT::Get::List-Cleanup="false" \
      -o Dir::Cache=$aptroot \
      -o Dir::State=$aptroot \
      -o Dir::State::status=$aptroot/status \
      -o Dir::Etc::SourceList=$aptroot/sources.list \
      -o APT::Architecture="amd64" \
      -o APT::Install-Recommends="false" \
      -o APT::Immediate-Configure="false" \
      -o Aptitude::CmdLine::Fix-Broken="true" \
      -o Aptitude::CmdLine::Assume-Yes="true" \
      --allow-untrusted -v -f -y --full-resolver install
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
apt-get \
    -o APT::Get::List-Cleanup="false" \
    -o Dir::Cache=$aptroot \
    -o Dir::State=$aptroot \
    -o Dir::State::status=$aptroot/status \
    -o Dir::Etc::SourceList=$aptroot/sources.list \
    -o APT::Install-Recommends="false" \
    -o APT::Architecture=amd64 \
    -o APT::Immediate-Configure="false" \
    update
}

cleanup() {
  rm -Rf $aptroot
}

packages=$1
status=$2
shift 2
action=$1
shift 1
#request=$@
#request=`cat Request`

mkdir -p $aptroot
mkdir -p $aptroot/lists
mkdir -p $aptroot/archives
mkdir -p $aptroot/archives/partial
mkdir -p $aptroot/lists/partial

initapt $packages $status
case "$1" in
  apt-get)
    fakeapt $action;
    ;;
  aptitude)
    fakeaptitude $action;
    ;;
  *)
    echo "solver not specified";
    ;;
esac
#fixfakeaptitude
#fakeaptitude $action
cleanup
