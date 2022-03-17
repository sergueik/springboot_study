#!/bin/ash

launch_xvfb() {
  # Set defaults if the user did not specify envs.
  export DISPLAY=${XVFB_DISPLAY:-:1}
  local SCREEN=${XVFB_SCREEN:-0}
  local SCREEN_RESOLUTION=${XVFB_RESOLUTION:-1280x1024x16}
  local TIMEOUT=${XVFB_TIMEOUT:-5}

  # Start and wait for either Xvfb to be fully up,
  # or we hit the TIMEOUT.
  Xvfb ${DISPLAY} -screen ${SCREEN} ${SCREEN_RESOLUTION} &
  local LOOP_COUNT=0
  until xdpyinfo -display ${DISPLAY} > /dev/null 2>&1
  do
    LOOP_COUNT=$((LOOP_COUNT+1))
    sleep 1
    if [ ${LOOP_COUNT} -gt ${TIMEOUT} ]
    then
      echo '[ERROR] xvfb failed to start.'
      exit 1
    fi
  done
}

launch_window_manager() {
#  local TIMEOUT=${XVFB_TIMEOUT:-5}

  # Start and wait for either fluxbox to be fully up or we hit
  # the TIMEOUT.
  fluxbox &
#  local LOOP_COUNT=0
#  until wmctrl -m > /dev/null 2>&1
#  do
#    LOOP_COUNT=$((LOOP_COUNT+1))
#    sleep 1
#    if [ ${LOOP_COUNT} -gt ${TIMEOUT} ]
#    then
#      echo "${G_LOG_E} fluxbox failed to start."
#      exit 1
#    fi
#  done
}

run_vnc_server() {
  local PASSWORD_ARG='-nopw'

  if [ -n "${VNC_SERVER_PASSWORD}" ]
  then
    local PASSWORD_ANSWER_FILEPATH="${HOME}/x11vnc.pass"
    if ! x11vnc -storepasswd "${VNC_SERVER_PASSWORD}" "${PASSWORD_ANSWER_FILEPATH}"
    then
      echo "[ERROR] Failed to store x11vnc password."
      exit 1
    fi
    PASSWORD_ARG=-"-rfbauth ${PASSWORD_ANSWER_FILEPATH}"
    echo "[INFO] The VNC server will ask for a password."
  else
    echo "[WARN] The VNC server will NOT ask for a password."
  fi

  x11vnc -display ${DISPLAY} -forever ${PASSWORD_ARG} &
  wait $!
}

launch_xvfb
launch_window_manager
run_vnc_server

