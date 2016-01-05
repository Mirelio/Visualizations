
"""
Sample from posterior 1
is it in posterior 2?
"""
import numpy



def posterior_range(post):

    #post = numpy.delete(data, 0, 1)
    pmin = numpy.amin(post, axis=0)
    pmax = numpy.amax(post, axis=0)
    return zip(pmin, pmax)


def sample(numb_to_samp, range_samp):

    samples_list = []
    partic_indic = 0

    while partic_indic <= numb_to_samp:
        samples = []
        for i in range_samp:
            samples.append(numpy.random.uniform(low=i[0], high=i[1]))
        samples_list.append(samples)
        partic_indic += 1
        if partic_indic == numb_to_samp:
            break
    return samples_list


def accept(samples_list, post_range, accepted, rejected):
    samp_counter = -1

    acc_tot =[]
    rej_tot = []

    #For each sample
    for samp in samples_list:
        samp_counter += 1
        flag = []
        #For each parameter
        for j in range(len(samp)):
            if samp[j] > post_range[j][0] and samp[j] < post_range[j][1]:
                if len(accepted[j]) == 0:
                    accepted[j] = [samp[j]]
                else:
                    accepted[j].append(samp[j])
                flag.append(1)
            else:
                if len(rejected[j]) == 0:
                    rejected[j] = [samp[j]]
                else:
                    rejected[j].append(samp[j])
                flag.append(0)

    for p in range(len(accepted)):
        if len(accepted[p]) == 0:
            accepted[p] = ['NA']
    for r in range(len(rejected)):
        if len(rejected[r]) == 0:
            rejected[r] = ['NA']
        #if 0 not in flag:
        #    acc_tot.append(samp_counter)
        #else:
        #    rej_tot.append(samp_counter)

    return accepted, rejected

data_bi = numpy.genfromtxt('Parameter_values_final_bistable.txt', delimiter=" ")
data_tri = numpy.genfromtxt('Parameter_values_final_tristable.txt', delimiter=" ")
numb_to_samp = 10000
range_bi = posterior_range(data_bi)
range_tri = posterior_range(data_tri)
samples_list_bi = sample(numb_to_samp, range_bi)
samples_list_tri = sample(numb_to_samp, range_tri)

#accepted_bi = [[], [], [], [], [], [], [], [], [], []]
#accepted_tri = [[], [], [], [], [], [], [], [], [], []]
#rejected_tri = [[], [], [], [], [], [], [], [], [], []]
#rejected_bi = [[], [], [], [], [], [], [], [], [], []]

bistable=[[], [], [], [], [], [], [], [], [],[], [], [], []]
tristable=[[], [], [], [], [], [], [], [], [],[], [], [], []]
both=[[], [], [], [], [], [], [], [], [],[], [], [], []]

samp_counter = -1
for samp in samples_list_bi:
    samp_counter += 1
    #For each parameter
    for j in range(len(samp)):
        if samp[j] > range_tri[j][0] and samp[j] < range_tri[j][1]:
            if len(both[j]) == 0:
                both[j] = [samp[j]]
                bistable[j] = [samp[j]]
                tristable[j] = [samp[j]]
            else:
                both[j].append(samp[j])
                tristable[j].append(samp[j])
                bistable[j].append(samp[j])
        else:
            if len(bistable[j]) == 0:
                bistable[j] = [samp[j]]
            else:
                bistable[j].append(samp[j])
for samp in samples_list_tri:
    samp_counter += 1
    #For each parameter
    for j in range(len(samp)):
        if samp[j] > range_bi[j][0] and samp[j] < range_bi[j][1]:
            if len(both[j]) == 0:
                both[j] = [samp[j]]
                bistable[j] = [samp[j]]
                tristable[j] = [samp[j]]
            else:
                tristable[j].append(samp[j])
                bistable[j].append(samp[j])
                both[j].append(samp[j])
        else:
            if len(bistable[j]) == 0:
                tristable[j] = [samp[j]]
            else:
                tristable[j].append(samp[j])



for p in range(len(bistable)):
    if len(bistable[p]) == 0:
        bistable[p] = ['NA']
for r in range(len(tristable)):
    if len(tristable[r]) == 0:
        tristable[r] = ['NA']
for r in range(len(both)):
    if len(both[r]) == 0:
        both[r] = ['NA']
with open('bistable_res.txt', 'w') as fb:
    fb.writelines('\t'.join(str(j) for j in i) + '\n' for i in bistable)
with open('tristable_res.txt', 'w') as ftr:
    ftr.writelines('\t'.join(str(j) for j in i) + '\n' for i in tristable)
with open('both_res.txt', 'w') as fbt:
    fbt.writelines('\t'.join(str(j) for j in i) + '\n' for i in both)

#accepted_bi, rejected_bi = accept(samples_list_bi, range_tri, accepted_bi, rejected_bi)
#accepted_tri, rejected_tri = accept(samples_list_tri, range_bi, accepted_tri, rejected_tri)
#save_results_files(accepted_bi, accepted_tri, rejected_bi, rejected_tri)

#print map(len, accepted_bi)
#print len(accepted_bi)


#Priors
#lims_tri = [['uniform', 3, 5], ['uniform', 3, 5], ['uniform', 0, 0.2], ['uniform', 0, 0.2],
#    ['uniform', 0, 2], ['uniform', 0, 2], ['uniform', 100, 200], ['uniform', 100, 200],
#    ['uniform', 0, 0.2], ['uniform', 0, 0.2], ['uniform', 2, 4], ['uniform', 2, 4],
#    ['uniform', 90, 110], ['uniform', 90, 110], ['uniform', 8, 12], ['uniform', 8, 12]]

#lims_bi = [['uniform', 35, 45], ['uniform', 35, 45], ['uniform', 0, 0.2], ['uniform', 0, 0.2],
#    ['uniform', 2, 4], ['uniform', 2, 4], ['uniform', 150, 250], ['uniform', 150, 250],
#    ['uniform', 0, 0.2], ['uniform', 0, 0.2]]
