#!/bin/bash

source .env/bin/activate
cd risk
python risk_server.py&
cd ..
cd cors/
python cors_server.py
cd ..
cd llm-rules
python llm_socket_server.py

