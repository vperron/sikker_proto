#!/usr/bin/python


"""
This tool is intended to connect to the given Sikker-Domozen service and 
eventually send it random bracelet statistics at a given rate.

It may also be used to retrieve some client parameters from time to time.

Victor Perron 2011 - TELECOM ParisTech

"""


import httplib
import json
import random
import math
import sys
import time
import hashlib


REST_URL    = "localhost:8080"
REST_PORT   = 8080
XFER_RATE   = 10 # Lines per second
HEADERS     = {"Content-Type" : "application/json"}
KEY         = "petitpapanoel" # Dummy key chosen here for all those customers
                              # Better keys are generated upon customer creation

IDs         = ["f57fa88aa6420869174078e941017b81",
               "f57fa88aa6420869174078e9410168a1",
               "f57fa88aa6420869174078e94101573e",
               "f57fa88aa6420869174078e9410149fa",
               "f57fa88aa6420869345078e94101025a",
               "f57fa88aa6420869174078e9410136dd",
               "f57fa88aa6420869174078e94101025a"]


def gauss(mean, deviation):
    return int(math.floor(random.gauss(mean,deviation)))

def addCookie(headers, cookie):
    newHeaders = dict(headers)
    newHeaders.update({"cookie": cookie})
    return newHeaders


def main():

    h = httplib.HTTPConnection(REST_URL)
    
    data = {}

    random.seed()


    while True:
        try:
            for _id in IDs:

                # Authentication
                h.request("GET", "/api/auth/"+_id)
                answer = h.getresponse()
                cookie = dict(answer.getheaders())['set-cookie']
                jsonAnswer = json.loads( "{"+answer.read()+"}" )
                m = hashlib.sha1()
                m.update(jsonAnswer["challenge"]+KEY)
                encrypted = m.hexdigest()
                encodedJson = {"challenge" : encrypted }
                h.request("PUT", "/api/auth", json.dumps(encodedJson), addCookie(HEADERS, cookie) )
                answer = h.getresponse()
                token = json.loads("{"+answer.read()+"}")['auth_token']



                data["bracelet_id"] = _id
                data["temperature"] = gauss(37, 1)
                data["cardio"]      = gauss(80, 4)
                data["accel"]       = gauss(5, 1)
                data["noise"]       = gauss(30, 3)
                data["auth_token"]  = token
                h.request("PUT", "/api/stats", json.dumps(data), addCookie(HEADERS, cookie))
                answer = h.getresponse()
                answer.read()


            sys.stdout.write(".")
            sys.stdout.flush()
            time.sleep(XFER_RATE)

        except KeyboardInterrupt:
            break

    
    print "Done."

if __name__ == "__main__":
    main()
