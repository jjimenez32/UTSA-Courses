#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <arpa/inet.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <sys/fcntl.h>
#include "pcap.h"
#include "dnet.h"
#include "prtlog.h"

int main (int argc, char *argv[]){
	char buf[4096];
	int filein;
	int c = 0;
	int firsttime;
	int b_sec,c_sec;
	unsigned int b_usec,c_usec;
	struct pcap_file_header head;
	struct pcap_pkthdr pkt;
	struct eth_hdr ethhead;
	struct arp_hdr *arphead;
	struct ip_hdr *iphead;
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
		printf("Packet %d\n", c);
		if (firsttime){
			firsttime =0;
			b_sec = pkt.ts.tv_sec;
			b_usec = pkt.ts.tv_sec;}
		c_sec = (unsigned)pkt.ts.tv_sec - b_sec;
		c_usec = (unsigned)pkt.ts.tv_sec - b_usec;
		
		while (b_usec < 0){
			c_usec += 1000000;
			c_sec--;}
		printf("%05u.%06u\n", (unsigned)c_sec,(unsigned)c_usec);
		printf("Captured Packet Length = %d\n",pkt.caplen);
		printf("Actual Packet Length = %d\n",pkt.len);
		c++;
		
		read(filein, &ethhead,sizeof( struct eth_hdr));
		read(filein, &buf,(pkt.len - sizeof(struct eth_hdr)));
		
		if (ntohs(ethhead.eth_type) == ETH_TYPE_IP){
			arphead = (struct arp_hdr *) buf;
			printf("Ethernet Header\n");
			printf("	ARP\n");
			printf("	arp operation = ");
			printarp((int)ntohs(arphead->ar_op));
			printf("\n");}
		else if (ntohs(ethhead.eth_type) == ETH_TYPE_IP){
			iphead = (struct ip_hdr *) buf;
			printf("Ethernet Header\n");
			printf("	IP\n");
			printf("	");
			printip((int)iphead->ip_p);
			printf("\n");}
		else printf("\n");
	}
	close(filein);
exit(0);

}

void printip(int x){
	switch(x){
		case 1:
			printf("ICMP");
			break;
		case 2:
			printf("IGMP");
			break;
		case 6:
			printf("TCP");
			break;
		case 17:
			printf("UDP");
			break;
		default: printf("UNRECOGNIZED\n");
	}
}

void printarp(int x){
	switch(x){
		case ARP_OP_REQUEST:
			printf("Arp Request");
			break;
		case ARP_OP_REPLY:
			printf("Arp Reply");
			break;
		case ARP_OP_REVREQUEST:
			printf("Arp Revrequest");
			break;
		case ARP_OP_REVREPLY:
			printf("Arp Revreply");
			break;
		default:
			printf("UNRECOGNIZED\n");
	}
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
			printf("UNRECOGNIZED\n");
	}
}	


