#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cs3723p1.h"

/**************************smrcAllocate***********************************
	void *smrcAllocate(StorageManager *pMgr, short shDataSize, short shNodeType,
		char sbData[], SMResult *psmResult);
ss

Purpose:
	Allocates memory to the new node created. Sets reference count to 1
Parameters:
	I StorageManager	*pMgr			Provides metadata about the user data and
							information for storage management.
	I short			shDataSize		Data size to be allocated.
	I short			shNodeType		Node type of the data.
	O char			sbData[]		Binary data set by this function. 
	O SMResult		*psmResult		Result structure containing a return code and
							an error message. For normal execution,
							the rc should be set to zero. 

Returns:
	pNode			Pointer to the allocated memory	
	NULL			If the memory was not allocated
Notes:

**************************************************************************/
void * smrcAllocate(StorageManager *pMgr , short shDataSize, short shNodeType, char sbData[], SMResult *psmResult)
{
	AllocNode 		*pNewNode;		//Pointer to a new node that will be created
	short 			shTotalSize;		//Holds size needed to allocate memory to pNewNode 
	
	//Determines the size of shDataSize 
	shTotalSize = shDataSize + 3 * sizeof(short);
	//Allocates the memory needed determined by shTotalSize
	pNewNode = smAlloc(pMgr, shTotalSize);

	//Error Check: Sees if memory is allocated. Returns NULL otherwise. 
	if (!pNewNode)
	{
		psmResult->rc = RC_NOT_AVAIL;
		return NULL;
	}

	//The following will initalize the parameters to the newly created node
	pNewNode->shNodeType = shNodeType;
	pNewNode->shAllocSize = shTotalSize;
	pNewNode->shRefCount = 1;
	memcpy(pNewNode->sbData, sbData, shDataSize);

	pNewNode = (AllocNode *)(((char *)pNewNode) + sizeof(short)*3);
	return (pNewNode);

}
/******************************smrcRemoveRef*******************************************
	void *smrcRemoveRef(StorageManager *pMgr,void *pUserData , SMResult *psmResult);
Purpose:
	Removes reference to the node. Decreases reference count by 1
Parameters:
	I StorageManager	*pMgr			vProvides metadata about the user data and
							information for storage management.
	I void 			*pUserData		Points to the user data (which is after the beginning
							of the allocated node).
	O SMResult		*psmResult		Result structure containing a return code and
							an error message. For normal execution,
							the rc should be set to zero. 
Returns:
	N/A
Notes:

***************************************************************************************/
void smrcRemoveRef(StorageManager *pMgr , void *pUserData, SMResult *psmResult)
{


	MetaAttr	*pMetaAttr;			//Pointer to MetaData Attribute
	void 		**pAttr;			//Double pointer to sbData
	AllocNode	*pNewNode;			//Pointer to pUserData
	
	pNewNode = (AllocNode *)(((char *)pUserData) - sizeof(short)*3);
	//Removes reference by decrementing reference count
	pNewNode->shRefCount--;
	//The following will check if the node's RefCount is 0 and see if anything else was referencing it.
	if (pNewNode->shRefCount == 0)
	{
		//Using the for loop condition from sbData to loop through the attributes of the node.
		for (int iAt = pMgr->nodeTypeM[pNewNode->shNodeType].shBeginMetaAttr; pMgr->metaAttrM[iAt].shNodeType == pNewNode->shNodeType; iAt++)
		{
			
			pMetaAttr = &(pMgr->metaAttrM[iAt]);
			//Checks if the MetaData attribute is a pointer
			if(pMetaAttr->cDataType == 'P')
			{
				//Double pointer will now point to the current attribute
				pAttr = (void **)&(pNewNode->sbData[pMetaAttr->shOffset]);
				//Error Check: Sees if the address is in the heap. Sets prmResult to proper error code otherwise. 
				if (!pAttr)
				{
					psmResult->rc = RC_INVALID_ADDR;
					strcpy(psmResult->szErrorMessage, "Adress is not in the heap ");
				}
				//Recursively calls this function if the pointer is not pointing to NULL
				if (*pAttr != NULL)
					smrcRemoveRef(pMgr, *pAttr, psmResult);
			}
		}
		
		//Invoke smFree to set refcount to -1
		smFree(pMgr, pNewNode,psmResult);
	}
}
/********************************smrcAssoc*********************************************
	void *smrcAllocate(StorageManager *pMgr, short shDataSize, short shNodeType,
		 SMResult *psmResult);
Purpose:
	Accociates node to another. Increments reference count
Parameters:
	I StorageManager	*pMgr			vProvides metadata about the user data and
							information for storage management.
	I short			shDataSize		Data size to be allocated.
	I short			shNodeType		Node type of the data.
	O SMResult		*psmResult		Result structure containing a return code and
							an error message. For normal execution,
							the rc should be set to zero. 

Returns:
	N/A
Notes:

***************************************************************************************/
void smrcAssoc(StorageManager *pMgr , void *pUserDataFrom, char szAttrName[], void *pUserDataTo, SMResult *psmResult)
{

	AllocNode	*pNodeFrom;			//pointer to pUserDataFrom
	MetaAttr	*pMetaAttr;			//pointer to MetaData Attribute
	void		**pAttr;			//double pointer to sbData location
	
			
	pNodeFrom = (AllocNode *)(((char *)pUserDataFrom) - sizeof(short)*3);
	//Using for loop condition from sbData to loop through the attributes of the node.
	for (int iAt = pMgr->nodeTypeM[pNodeFrom->shNodeType].shBeginMetaAttr; pMgr->metaAttrM[iAt].shNodeType == pNodeFrom->shNodeType; iAt++)
	{
		
		pMetaAttr = &(pMgr->metaAttrM[iAt]);
		//Compares attribute names to see if it was found.
		if (strcmp(pMetaAttr->szAttrName, szAttrName) == 0)
		{
			//Checks to see if attribute is a pointer.
			if(pMetaAttr->cDataType == 'P')
			{
				//Double pointer will now point to current node.
				pAttr = (void **)&pNodeFrom->sbData[pMetaAttr->shOffset];
				//Error Check:Sees if address is in the heap. Will set psmResult to proper error code otherwise.
				if (!pAttr)
				{
					psmResult->rc = RC_INVALID_ADDR;
					strcpy(psmResult->szErrorMessage, "Adress is not in the heap ");
				}
				//Calls smrcRemoveRef if pointer is already referencing something
				if (*pAttr != NULL)
					smrcRemoveRef(pMgr, *pAttr, psmResult);
				
				//Set pointer to pUserDataTo then add reference count by 1.
				*pAttr = pUserDataTo;
				smrcAddRef(pMgr, pUserDataTo, psmResult);
			}
			else
				psmResult->rc = RC_ASSOC_ATTR_NOT_PTR;
		}
	}

}
/*********************************smrcAddRef*****************************************
	void *smrcAddRef(StorageManager *pMgr, void *pUserDataTo, SMResult *psmResult);
Purpose:
	Adds a reference to specified user pointer.
Parameters:
	I StorageManager	*pMgr			vProvides metadata about the user data and
							information for storage management.
	I void 			*pUserDataTo	
	O SMResult		*psmResult		Result structure containing a return code and
							an error message. For normal execution,
							the rc should be set to zero. 

Returns:
	N/A
Notes:
*************************************************************************************/
void smrcAddRef(StorageManager *pMgr , void *pUserDataTo, SMResult *psmResult)
{

	AllocNode 	*pNodeTo;			//Pointer to pUserDataTo		
	
	pNodeTo = (AllocNode *)(((char *)pUserDataTo) - sizeof(short) * 3);
	
	//Error Check
	if (!pNodeTo)
		psmResult->rc = RC_INVALID_ADDR;
	
	//Adds reference by incrementing the reference count. 
	pNodeTo->shRefCount++;

}
/**********************************printNode******************************************
	void printNode(StorageManager *pMgr, void *pUserData);
Purpose:
	Prints all metadata of the node that is being pointed to
Parameters:
	I StorageManager	*pMgr			vProvides metadata about the user data and
							information for storage management.
	I void 			*pUserData		Points to the user data (which is after the beginning
							of the allocated node).

Returns:
	N/A
Notes:
	
**********************************************************************************/
void printNode(StorageManager *pMgr, void *pUserData)
{
	MetaAttr 	*pMetaAttr;			//Pointer to MetaData Attribute
	AllocNode	*pNewNode;			//Pointer to pUserData
	void 		**pContent;			//Double pointer meant for pointing to content

	pNewNode = (AllocNode*)(((char *)pUserData) - sizeof(short)*3);

	
	printf( "%4s %-10s %5s %10s %8s %10s\n","", "Alloc Adress", "Size", "Node Type", "Ref Cnt", "Data Adress");
	printf ("%4s %-10p %5d %9d %9d %13p\n","", pNewNode, pNewNode->shAllocSize, pNewNode->shNodeType, pNewNode->shRefCount, pUserData);
	printf("%12s %-12s %5s %2s\n","", "Attr Name", "Type", "Value");
	
	//Using sbData for loop condition to loop through all attributes of the node.
	for (int iAt = pMgr->nodeTypeM[pNewNode->shNodeType].shBeginMetaAttr; pMgr->metaAttrM[iAt].shNodeType == pNewNode->shNodeType; iAt++)

	{
		pMetaAttr = &(pMgr->metaAttrM[iAt]);
		pContent = (void **)&pNewNode->sbData[pMetaAttr->shOffset];
		printf("%12s %-15s %-c\t","",pMetaAttr->szAttrName, pMetaAttr->cDataType);

		//Prints the MetaAttribute of the specified datatype.
		if( pMetaAttr->cDataType == 'P')
		{
			printf("%-10p\n" ,*pContent);			
		}
		else if( pMetaAttr->cDataType == 'S')
		{
			printf("%-10s\n",(char *) pContent);
		}
		else if( pMetaAttr->cDataType == 'D')
		{
			//creating double ptr to typecast pContent
			double  *dContent;
			dContent = (double *) pContent;
			printf( "%-10f\n", *dContent);
		}
		else if( pMetaAttr->cDataType == 'I')
		{
			//creating int ptr to typecast pContent
			int *iContent;
			iContent = (int *)pContent;
			printf("%-10d\n", *iContent);
		}
	}
}
