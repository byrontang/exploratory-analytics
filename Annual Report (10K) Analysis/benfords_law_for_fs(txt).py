# -*- coding: utf-8 -*-
import numpy as np

benford = np.array([.301, .176, .125, .097, .079, .067, .058, .051, .046])

def read_txt(txt_file_name):
    a_file = list()
    with open(txt_file_name, 'r') as f:
        for line in f:
            for word in line.split():
                a_file.append(word)
    return a_file

def is_number(var):
    try:
        if int(var):
            return True
    except Exception:
        return False

def count_number(num, a_list):
    new_list = list()
    for i in range(0, num):
        s = a_list[i]
        s = s.replace(',', '')            
        s = s.replace('$', '')
        s = s.replace('(', '')
        s = s.replace(')', '')
        s = s.replace('%', '')
        if is_number(s):
            new_list.append(abs(int(s)))
    return new_list

def test_benford(a_list):   
    temp_list = [int(str(num * 1000000)[0]) for num in a_list]
    list_count = list()
    for i in range(1,10):
        list_count.append(float(temp_list.count(i))/len(temp_list))
    benford_test = max(abs(benford - np.array(list_count)))
    return benford_test
    
hess_10K = read_txt('HESS.txt')
hess_numbers = count_number(len(hess_10K), hess_10K)
hess_benford = test_benford(hess_numbers)

marathon_10K = read_txt('Marathon.txt')
marathon_numbers = count_number(len(marathon_10K), marathon_10K)
marathon_benford = test_benford(marathon_numbers)

apc_10K = read_txt('APC.txt')
apc_numbers = count_number(len(apc_10K), apc_10K)
apc_benford = test_benford(apc_numbers)

hess_test = hess_benford < 1.36/np.sqrt(len(hess_numbers))
marathon_test = marathon_benford < 1.36/np.sqrt(len(marathon_numbers))
apc_test = apc_benford < 1.36/np.sqrt(len(apc_numbers))

print "Is HESS consistent with Benford’s law? {} KS:{:4f} / Benchmark:{:4f}".format(hess_test, hess_benford, 1.36/np.sqrt(len(hess_numbers)))
print "Is Marathon Oil consistent with Benford’s law? {} KS:{:4f} / Benchmark:{:4f}".format(marathon_test, marathon_benford, 1.36/np.sqrt(len(marathon_numbers)))
print "Is APC consistent with Benford’s law? {} KS:{:4f} / Benchmark:{:4f}".format(apc_test, apc_benford, 1.36/np.sqrt(len(apc_numbers)))