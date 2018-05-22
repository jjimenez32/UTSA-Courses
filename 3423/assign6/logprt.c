#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <sys/fcntl.h>
#include "logprt.h"



int main (int argc, char *argv[]){
	int filein;
	int count = 0;
	int firsttime;
	int b_usec;
	int c_usec;
	unsigned int b_sec;
	unsigned int c_sec;
	struct pcap_file_header head;
	struct pcap_pkthdr pkt;
	char buf[pkt.len];
	if (( filein = open(argv[1], O_RDONLY)) <0){
		perror(argv[1]);
		exit(-1);}
	read (filein, &head, sizeof(struct pcap_file_header));
	printpcap(head.magic);
	printf("\n");
	printf("Version major number = %d\n",head.version_major);
	printf("Version minor number = %d\n",head.version_minor);
	printf("GMT to local correction = %d\n",head.thiszone);
	printf("Timestap accuracy = %d\n",head.sigfigs);
	printf("Snaplen = %d\n",head.snaplen);
	printf("Linktype = %d\n",head.linktype);
	printf("\n");
	while ((read(filein, &pkt, sizeof(struct pcap_pkthdr)))>0){
		printf("Packet %d\n", count);
		
		if (firsttime ){
			firsttime =0;
			b_sec = pkt.ts.tv_sec;
			b_usec = pkt.ts.tv_usec;}
		c_sec = (unsigned)pkt.ts.tv_sec - b_sec;
		c_usec = (unsigned)pkt.ts.tv_usec - b_usec;
		while (c_usec < 0){
			c_usec += 1000000;
			c_sec--;}
		
		printf("%05u.%06u\n", (unsigned)c_sec,(unsigned)c_usec);
		printf("Captured Packet Length = %u\n", pkt.caplen);
		printf("Actual Packet Length = %u\n", pkt.len);
		printf("\n");
		count++;
		//read(filein,&buf,pkt.len);
	}
	exit(0);
}




void printpcap(int x){
	switch(x){
		case PCAP_MAGIC:
			printf("PCAP_MAGIC");
			break;
		case PCAP_MODIFIED_MAGIC:
			printf("PCAP_MODIFIED_MAGIC");
			break;
		case PCAP_SWAPPED_MAGIC:
			printf("PCAP_SWAPPED_MAGIC");
			break;
		case PCAP_SWAPPED_MODIFIED_MAGIC:
			printf("PCAP_SWAPPED_MODIFIED_MAGIC");
			break;
		default:
			printf("ERROR: No Match");
	}
}

