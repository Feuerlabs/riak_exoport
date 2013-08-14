#!/bin/sh

. $HOME/.exodmrc

if [ $# != 4 ]
then
    echo "Usage: $0 device-id parent batch-size previous (\"\" start from beginning)"
    exit 255
fi

curl -u $USER_AUTH -k -X POST $URL -d @- <<EOF
{
    "json-rpc": "2.0",
    "method": "riak-stats:list-children",
    "id": "1",
    "params": {
        "device-id": "$1",
        "parent": "$2",
	"n": $3
    }
}
EOF
echo
