## -*- coding: latin-1 -*-

#####################################################
"""
copy embargo_external_input(wholename,status) from 'D:\Dushyant\My Documents\Projects\Embargo\Test data\POC_Embargo.tsv'
CREATE TABLE public.embargo_poc_internal_input
(
customer_id character varying(100),
company_name character varying(1500),
address1 character varying(1500),
address2 character varying(1500),
zipcode character varying(50),
city character varying(50),
country character varying(50),
first_name character varying(100),
last_name character varying(100),
company_name_raw character varying(1500),
status character varying(50),
tvh_id serial
)
WITH (
OIDS=TRUE
);


CREATE TABLE public.embargo_poc_external_input
(
ex_company_name character varying(1500),
ex_address character varying(5000),
ex_name character varying(1000),
ex_alt_name character varying(1000),
feed_name character varying(1000),
feed_url character varying(1000),

ex_company_name_raw character varying(1500),
ex_address_raw character varying(5000),
ex_name_raw character varying(1000),
ex_alt_name_raw character varying(1000),

status character varying(50),
tvh_id serial
)
WITH (
OIDS=TRUE
);

CREATE TABLE public.embargo_score
(
customer_id character varying(500),
company_name character varying(1500),
name character varying(1000),
address character varying(1500),
zipcode character varying(50),
city character varying(50),
country character varying(50),
ex_company_name character varying(1500),
ex_address character varying(5000),
ex_name character varying(1000),

ex_company_name_raw character varying(1500),
ex_address_raw character varying(5000),
ex_name_raw character varying(1000),
ex_alt_name_raw character varying(1000),
company_name_raw character varying(1500),

feed_name character varying(1000),
feed_url character varying(1000),

company_name_score integer,
name_score integer,
address_score integer,
zipcode_score integer,
city_score integer,
country_score integer,
final_score integer,
status character varying(50),
tvh_id serial
)
WITH (
OIDS=TRUE
);

"""
#####################################################
import time
import re
# import requests
from bs4 import BeautifulSoup
import math
import psycopg2
import sys
# import RND
from datetime import datetime
from fuzzywuzzy import fuzz
from fuzzywuzzy import process
from pyjarowinkler import distance

if __name__ == '__main__':
    start_time = datetime.now()
    print(start_time)
    db = psycopg2.connect(host="localhost", user="postgres", password="sql", database="postgres", port=5432)
    db.commit()
    cursor = db.cursor()
    cursor.execute(
        "SELECT customer_id,company_name,address1,address2,zipcode,city,country,first_name,last_name,company_name_raw,tvh_id FROM embargo_poc_internal_input where status = 'input';")
    cust_list = cursor.fetchall()
    for coloumn in cust_list:
        customer_id = coloumn[0]
        company_name = coloumn[1]
        address1 = coloumn[2]
        address2 = coloumn[3]
        zipcode = coloumn[4]
        city = coloumn[5]
        country = coloumn[6]
        first_name = coloumn[7]
        last_name = coloumn[8]
        company_name_raw = coloumn[9]
        internal_tvh_id = coloumn[10]
        # print (internal_tvh_id)
        # sys.exit()
        name = ''
        if first_name != 'NA' and last_name != 'NA':
            if first_name != 'NA':
                name = first_name
            if last_name != 'NA':
                name = name + ' ' + str(last_name)
        else:
            name = 'NA'

        address = ''
        if address1 != 'NA' and address2 != 'NA':
            if address1 != 'NA':
                address = address1
            if address2 != 'NA':
                address = address + ' ' + str(address2)
        else:
            address = 'NA'

        print(internal_tvh_id)
        # if last_name == 'n/a':
        #    continue
        cursor.execute(
            "SELECT ex_company_name,ex_address,ex_name, ex_alt_name,feed_name , feed_url , ex_company_name_raw,ex_address_raw,ex_name_raw, ex_alt_name_raw, tvh_id FROM embargo_poc_external_input where status = 'crawl';")
        search_list = cursor.fetchall()
        for coloumn in search_list:
            ex_company_name = coloumn[0]
            ex_address = coloumn[1]
            ex_name = coloumn[2]
            ex_alt_name = coloumn[3]

            feed_name = coloumn[4]
            feed_url = coloumn[5]

            ex_company_name_raw = coloumn[6]
            ex_address_raw = coloumn[7]
            ex_name_raw = coloumn[8]
            ex_alt_name_raw = coloumn[9]

            tvh_id = coloumn[10]

            if ex_alt_name != 'NA' and ex_name != 'NA':
                ex_name = str(ex_name) + ' ' + str(ex_alt_name)
            elif ex_name != 'NA':
                ex_name = ex_alt_name

            company_name_score = 0
            name_score = 0
            address_score = 0
            zipcode_score = 0
            city_score = 0
            country_score = 0

            if company_name != 'NA':
                if ex_company_name != 'dk':
                    set1 = set(map(lambda word: word.lower(), company_name.split(' ')))
                    set2 = set(map(lambda word: word.lower(), ex_company_name.split(' ')))
                    # print(set2)
                    # print (set1)
                    common = set1 & set2
                    # print(common)
                    company_name_score = len(common) / len(set1)
                    company_name_score = int(math.ceil(company_name_score * 100))
                else:
                    ex_company_name = '<REFER ex_name coloumn>'
                    set1 = set(map(lambda word: word.lower(), company_name.split(' ')))
                    set2 = set(map(lambda word: word.lower(), ex_name.split(' ')))
                    # print(set2)
                    # print (set1)
                    common = set1 & set2
                    # print(common)
                    company_name_score = len(common) / len(set1)
                    company_name_score = int(math.ceil(company_name_score * 100))

                # print (company_name_score)
           # if company_name_score >= 50:
            if name != 'NA':
                set1 = set(map(lambda word: word.lower(), name.split(' ')))
                set2 = set(map(lambda word: word.lower(), ex_name.split(' ')))
                # print(set2)
                # print (set1)
                common = set1 & set2
                # print(common)
                name_score = len(common) / len(set1)
                name_score = int(math.ceil(name_score * 100))
                # print (name_score)

            if company_name_score > 50 or name_score >50:


                if address != 'NA':
                    set1 = set(map(lambda word: word.lower(), address.split(' ')))
                    set2 = set(map(lambda word: word.lower(), ex_address.split(' ')))
                    # print(set2)
                    # print (set1)
                    common = set1 & set2
                    # print(common)
                    address_score = len(common) / len(set1)
                    address_score = int(math.ceil(address_score * 100))
                    # print (address_score)

                if zipcode != 'NA':
                    set1 = set(map(lambda word: word.lower(), zipcode.split(' ')))
                    set2 = set(map(lambda word: word.lower(), ex_address.split(' ')))
                    # print(set2)
                    # print (set1)
                    common = set1 & set2
                    # print(common)
                    zipcode_score = len(common) / len(set1)
                    zipcode_score = int(math.ceil(zipcode_score * 100))
                    # print (zipcode_score)

                if city != 'NA':
                    set1 = set(map(lambda word: word.lower(), city.split(' ')))
                    set2 = set(map(lambda word: word.lower(), ex_address.split(' ')))
                    # print(set2)
                    # print (set1)
                    common = set1 & set2
                    # print(common)
                    city_score = len(common) / len(set1)
                    city_score = int(math.ceil(city_score * 100))
                    # print (city_score)

                if country != 'NA':
                    set1 = set(map(lambda word: word.lower(), country.split(' ')))
                    set2 = set(map(lambda word: word.lower(), ex_address.split(' ')))
                    # print(set2)
                    # print (set1)
                    common = set1 & set2
                    # print(common)
                    country_score = len(common) / len(set1)
                    country_score = int(math.ceil(country_score * 100))
                    # print (country_score)

#ex_company_name_raw,ex_address_raw,ex_name_raw, ex_alt_name_raw,company_name_raw,feed_name,feed_url
                try:
                    # print ("i am here")
                    db.commit()
                    # cursor.execute(
                    #     "INSERT INTO embargo_score (customer_id,company_name,name,address,zipcode,city,country,ex_company_name,ex_address,ex_name,ex_company_name_raw,ex_address_raw,ex_name_raw, ex_alt_name_raw,company_name_raw,feed_name,feed_url,company_name_score,name_score,address_score,zipcode_score,city_score,country_score,status) VALUES (%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s);",
                    #     (customer_id, company_name, name, address, zipcode, city, country, ex_company_name, ex_address,
                    #      ex_name,ex_company_name_raw,ex_address_raw,ex_name_raw, ex_alt_name_raw,company_name_raw,feed_name,feed_url, company_name_score, name_score, address_score, zipcode_score, city_score, country_score,
                    #      'done'))
                    cursor.execute(
                        "INSERT INTO embargo_score (customer_id,company_name_raw,name,address,zipcode,city,country,ex_company_name_raw,ex_address_raw,ex_name_raw, ex_alt_name_raw,feed_name,feed_url,company_name_score,name_score,address_score,zipcode_score,city_score,country_score,status) VALUES (%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s);",
                        (customer_id,company_name_raw,name,address,zipcode,city,country,ex_company_name_raw,ex_address_raw,ex_name_raw, ex_alt_name_raw,feed_name,feed_url,company_name_score,name_score,address_score,zipcode_score,city_score,country_score,
                         'done'))
                    cursor.execute("update embargo_poc_external_input set status = %s where tvh_id=%s",
                                   ("executed", tvh_id))
                    db.commit()
                    # sys.exit()
                    print("Record Inserted")
                    # sys.exit()
                except Exception as e:
                    print(e)

        cursor.execute("update embargo_poc_external_input set status = %s ", ("crawl",))
        print(internal_tvh_id)
        # sys.exit()
        cursor.execute("update embargo_poc_internal_input set status = %s where tvh_id=%s",
                       ("input_executed", internal_tvh_id))
        db.commit()
        end_time1 = datetime.now()
        print('>>>', end_time1 , '>>>>>>>>>>>>')

        # cursor.execute("update embargo_poc_external_input set status = %s ",("crawl",))
        # sys.exit()
    end_time = datetime.now()
    print('>>>', start_time)
    print('>>>', end_time)