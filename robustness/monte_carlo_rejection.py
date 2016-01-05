import read_input
import numpy

def sample_priors(numb_to_samp):
    samples_list = []
    partic_indic = 0

    while partic_indic <= numb_to_samp:
        samples = []
        for i in read_input.lims:
            if i[0] == 'constant':
                samples.append(0)
            elif i[0] == 'uniform':
                samples.append(numpy.random.uniform(low=i[1], high=i[2]))
                #segm = (float(i[2]) - float(i[1]))/numb_to_samp
                #samples.append(numpy.random.uniform(low=(float(i[1])+((partic_indic)*segm)), high=(float(i[1])+((partic_indic+1)*segm))))
        samples_list.append(samples[1:])
        partic_indic += 1
        if partic_indic == numb_to_samp:
            break
    return samples_list

def posterior_range():
    data = numpy.genfromtxt('gard_det/results_deter_high_mean/Parameter_values_final.txt', delimiter=" ")
    post = numpy.delete(data, 0, 1)
    pmin = numpy.amin(post, axis=0)
    pmax = numpy.amax(post, axis=0)
    return zip(pmin, pmax)

def accept_reject_allD(samples_list, post_range, numb_to_samp):
    samp_counter = -1
    accepted = []
    rejected = []
    for samp in samples_list:
        samp_counter += 1
        flag = []
        for j in range(len(samp)):
            if samp[j] > post_range[j][0] and samp[j] < post_range[j][1]:
                flag.append(1)
            else:
                flag.append(0)
        if 0 not in flag:
            accepted.append(samp_counter)
    acc_rate = float(len(accepted))/numb_to_samp
    return acc_rate

numb_to_samp = 1000
samples_list = sample_priors(numb_to_samp)
post_range = posterior_range()
robust_measure = accept_reject_allD(samples_list, post_range, numb_to_samp)
print robust_measure
#acc_rate = accept_reject_ND(samples_list, post_range)

