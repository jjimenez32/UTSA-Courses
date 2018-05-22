#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>
#include <string.h>
#include <sys/queue.h>

#define NR_PHASES 4

pthread_t cpu_t[8];
pthread_t io_t[4];
pthread_t submit_t[4];

int breakout = 0;
int operate_breakout = 0;
int curr_jobid = 0;
int finished_queue_length = 0;

pthread_mutex_t id_lock;
pthread_mutex_t cpu_lock;
pthread_mutex_t io_lock;
pthread_mutex_t finished_lock;
pthread_mutex_t breakout_lock;
pthread_mutex_t operate_breakout_lock;

TAILQ_HEAD(l1, job) ready;
TAILQ_HEAD(l2, job) io;
TAILQ_HEAD(l3, job) finished;

struct l1 *lh1;
struct l2 *lh2;
struct l3 *lh3;
struct job {
	int job_id;
	unsigned int nr_phases;
	int current_phase;
	unsigned int phasetype[NR_PHASES]; // 0 == CPU, 1 == IO
	unsigned int duration[NR_PHASES];
	int is_completed; //0 = not completed, 1 is completed

	TAILQ_ENTRY(job) jobs;
} job;

typedef struct worked_on_job {
	struct job *curr_job;
	int has_job;
} worked_on_job;

void enqueue_cpu_job(void *the_job) {
	struct job *new_job = (struct job *)the_job;
	TAILQ_INSERT_TAIL(&ready, new_job, jobs);
	printf("\nENQUEUED A CPU JOB!\n");
}

void enqueue_io_job(void *the_job) {
	struct job *new_job = (struct job *)the_job;
	TAILQ_INSERT_TAIL(&io, new_job, jobs);
	printf("\nENQUEUED AN IO JOB!\n");
}

void enqueue_finished_job(void *the_job) {
	struct job *new_job = (struct job *) the_job;
	TAILQ_INSERT_TAIL(&finished, new_job, jobs);
	printf("\nENQUEUED A FINISHED JOB!\n");
}
unsigned int rand_interval(unsigned int min, unsigned int max)
{
	int r;
	const unsigned int range = 1 + max - min;
	const unsigned int buckets = RAND_MAX / range;
	const unsigned int limit = buckets * range;

	do
	{
		r = rand();
	} while (r >= limit);

	return min + (r / buckets);
}

void *cpu_operate(void *arg) {
	worked_on_job *my_job = (worked_on_job *) arg;
	while(operate_breakout == 0) {
		if (finished_queue_length >= 10) {
			pthread_mutex_lock(&operate_breakout_lock);	
			operate_breakout = 1;
			pthread_mutex_unlock(&operate_breakout_lock);
			return;
		}
		struct job *next_job;
		pthread_mutex_lock(&cpu_lock);
		if (ready.tqh_first != NULL) {
			next_job = ready.tqh_first;
			TAILQ_REMOVE(&ready, ready.tqh_first, jobs);
			my_job->curr_job = next_job;
			my_job->has_job = 1;
			struct job *test = (struct job *) my_job->curr_job;
			printf("This CPU thread is doing #: %d\n", test->job_id);
		} 	
		pthread_mutex_unlock(&cpu_lock);
		
		if (my_job->has_job == 1) {
			struct job *my_curr_job = (struct job *) my_job->curr_job;
			int curr_phase = my_curr_job->current_phase;
			//int sleep_duration = my_curr_job->duration[curr_phase];
			//sleep(sleep_duration);
			printf("Operated on phase: %d of job id: %d\n", my_curr_job->current_phase, my_curr_job->job_id);
			my_curr_job->current_phase = curr_phase + 1;
			
			if (my_curr_job->current_phase == my_curr_job->nr_phases) {
				//enqueue to finished queue
				pthread_mutex_lock(&finished_lock);
				finished_queue_length = finished_queue_length + 1;
				enqueue_finished_job(my_curr_job);
				pthread_mutex_unlock(&finished_lock);
			} else if (my_curr_job->phasetype[curr_phase+1] == 0) {
				//enqueue CPU
				pthread_mutex_lock(&cpu_lock);
				enqueue_cpu_job(my_curr_job);
				pthread_mutex_unlock(&cpu_lock);
			} else if (my_curr_job->phasetype[curr_phase+1] == 1) {
				//enqueue IO
				pthread_mutex_lock(&io_lock);
				enqueue_io_job(my_curr_job);
				pthread_mutex_unlock(&io_lock);
			} else {
				printf("GOT HERE, BUT IT SHOULDNT");
			}
			my_job->has_job = 0;
		}
		//finished_queue_length = finished_queue_length + 1;		
	}
	return;
}

void *io_operate(void *arg) {
	worked_on_job *my_job = (worked_on_job *) arg;
	while(operate_breakout == 0) {
		if (finished_queue_length >= 10) {
			pthread_mutex_lock(&operate_breakout_lock);	
			operate_breakout = 1;
			pthread_mutex_unlock(&operate_breakout_lock);
			return;
		}
		struct job *next_job;
		pthread_mutex_lock(&io_lock);
		if (io.tqh_first != NULL) {
			next_job = io.tqh_first;
			TAILQ_REMOVE(&io, io.tqh_first, jobs);
			my_job->curr_job = next_job;
			my_job->has_job = 1;
			struct job *test = (struct job *) my_job->curr_job;
			printf("This IO thread is doing #: %d\n", test->job_id);
		} 	
		pthread_mutex_unlock(&io_lock);
		
		if (my_job->has_job == 1) {
			struct job *my_curr_job = (struct job *) my_job->curr_job;
			int curr_phase = my_curr_job->current_phase;
			//int sleep_duration = my_curr_job->duration[curr_phase];
			//sleep(sleep_duration);
			printf("Operated on phase: %d of job id: %d\n", my_curr_job->current_phase, my_curr_job->job_id);
			my_curr_job->current_phase = curr_phase + 1;
			
			if (my_curr_job->current_phase == my_curr_job->nr_phases) {
				//enqueue to finished queue
				pthread_mutex_lock(&finished_lock);
				finished_queue_length = finished_queue_length + 1;
				enqueue_finished_job(my_curr_job);
				pthread_mutex_unlock(&finished_lock);
			} else if (my_curr_job->phasetype[curr_phase+1] == 0) {
				//enqueue CPU
				pthread_mutex_lock(&cpu_lock);
				enqueue_cpu_job(my_curr_job);
				pthread_mutex_unlock(&cpu_lock);
			} else if (my_curr_job->phasetype[curr_phase+1] == 1) {
				//enqueue IO
				pthread_mutex_lock(&io_lock);
				enqueue_io_job(my_curr_job);
				pthread_mutex_unlock(&io_lock);
			} else {
				printf("GOT HERE, BUT IT SHOULDNT");
			}
			my_job->has_job = 0;
		}
		//finished_queue_length = finished_queue_length + 1;		
	}
	return;	
}

void *add_job(void *arg) {
	struct job *job;
	int i;
	while (breakout == 0) {
		job = (struct job *) malloc(sizeof(struct job));		
		pthread_mutex_lock(&id_lock);
		if(curr_jobid >= 10) {
			breakout = 1;
			return;
		} else {
			curr_jobid = curr_jobid + 1;
			job->job_id = curr_jobid;	
		}	
		pthread_mutex_unlock(&id_lock);
		job->current_phase = 0;
		
		for(i = 0; i < NR_PHASES; i++)
		{
			job->phasetype[i] = rand_interval(0, 1);
			job->duration[i] = rand_interval(1, 3);
		}
		job->nr_phases = NR_PHASES;
		job->is_completed = 0;	
		if (job->phasetype[0] == 0) {
			pthread_mutex_lock(&cpu_lock);
			enqueue_cpu_job(job);
			pthread_mutex_unlock(&cpu_lock);
		} else {
			pthread_mutex_lock(&io_lock);
			enqueue_io_job(job);
			pthread_mutex_unlock(&io_lock);
		}
		sleep(2);
	}
	return; 
}


int main(int argc, char *argv[]) {
	TAILQ_INIT(&ready);
	TAILQ_INIT(&io);
	TAILQ_INIT(&finished);
	
	worked_on_job cpu_jobs[8];
	worked_on_job io_jobs[4];

	if (pthread_mutex_init(&cpu_lock, NULL) != 0) {
		printf("\n cpu mutex t failed\n");
		return 1;
	} 

	if (pthread_mutex_init(&io_lock, NULL) != 0) {
		printf("\n io mutex t failed\n");
		return 1;
	}

	if (pthread_mutex_init(&finished_lock, NULL) != 0) {
		printf("\n finished mutex t failed\n");
		return 1;
	}
	
	if(pthread_mutex_init(&id_lock, NULL) != 0) {
		printf("\n finished mutex t failed\n");
		return 1;
	}
	
	if(pthread_mutex_init(&breakout_lock, NULL) != 0) {
		printf("\n breakout mutex t failed\n");
		return 1;
	}
	
	if(pthread_mutex_init(&operate_breakout_lock, NULL) != 0) {
		printf("\n breakout_operate mutex t failed\n");
		return 1;
	}

	int i;

	for (i = 0; i < 4; i++) {
		pthread_create(&submit_t[i], NULL, add_job, NULL);
	}
	
	for (i = 0; i < 8; i++) {
		cpu_jobs[i].has_job = 0;
		pthread_create(&cpu_t[i], NULL, cpu_operate, &cpu_jobs[i]);
	}

	for (i = 0; i <4; i++) {
		pthread_create(&io_t[i], NULL, io_operate, &io_jobs[i]);
	}

	
	for (i = 0; i <8; i++) {
		pthread_join(cpu_t[i], NULL);
	}

	for (i = 0; i < 4; i++) {
		pthread_join(io_t[i], NULL);
	}

	for (i = 0; i < 4; i++) {
		pthread_join(submit_t[i], NULL);
	}
	exit(0);
}





