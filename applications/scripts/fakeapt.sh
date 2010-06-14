#!/bin/bash
# Fri Jun 11 2010 Pietro Abate <pietro.abate@pps.jussieu.fr>

set -x 
aptroot='/var/tmp/fakeapt'

fakeapt() {
  action=$1
  shift 1
  request=$@
  apt-get -s \
      -o APT::Get::List-Cleanup="false" \
      -o Dir::Cache=$aptroot \
      -o Dir::State=$aptroot \
      -o Dir::State::status=$aptroot/status \
      -o Dir::Etc::SourceList=$aptroot/sources.list \
      -o APT::Install-Recommends="false" \
      -o APT::Architecture=amd64 \
      -o APT::Immediate-Configure="false" \
      $action \"$request\"
}

fakeaptitude() {
  action=$1
  shift 1
  request=$@
  aptitude -s \
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
      $action \"$request\"
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
apt-get -s \
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
request=$@

mkdir -p $aptroot
mkdir -p $aptroot/lists
mkdir -p $aptroot/archives
mkdir -p $aptroot/archives/partial
mkdir -p $aptroot/lists/partial

initapt $packages $status
#fakeapt $action $request
fakeaptitude $action $request
#cleanup
