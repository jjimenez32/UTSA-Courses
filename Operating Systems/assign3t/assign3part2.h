static unsigned long pageTable[7];
static unsigned long pageTimer[7];
static unsigned long freePage[7];

char setVirtualPage(unsigned long physicalPage, unsigned long virtualPage);
unsigned long getVirtualPage(unsigned long PhysicalPage);
unsigned long getPhysicalPage(unsigned long virtualPage);
unsigned long getTimer(unsigned long physicalPage);
void initializePageTable();
void incrementTimer();
void InitializePhysical();
unsigned long findFree();
unsigned long evictLRU();