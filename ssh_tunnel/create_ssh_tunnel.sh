#!/bin/bash
# Create reverse ssh tunnel to edulution google server
createTunnel() {
sshpass -p $SSHPASS /usr/bin/ssh -N -R 19999:localhost:22 edulution@130.211.93.74
if [[ $? -eq 0 ]]; then
echo Tunnel to Edulution created successfully
else
echo An error occurred creating a tunnel to Edulution
fi
}
/bin/pidof ssh
if [[ $? -ne 0 ]]; then
echo "Creating new tunnel connection..."
echo "Press CTRL + C or close this terminal to stop"
createTunnel
fi

