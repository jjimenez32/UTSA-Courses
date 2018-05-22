#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cs3723p1.h"

/**************************smrcAllocate***********************************
	void *smrcAllocate(StorageManager *pMgr, short shDataSize, short shNodeType,
		char sbData[], SMResult *psmResult);

Purpose:
	Allocates memory to the new node created. Sets reference count to 1
Parameters:
	I StorageManager	*pMgr		
	I short			shDataSize
	I short			shNodeType
	O char			sbData[]
	O SMResult		*psmResult

Returns:
	pNode			Pointer to the allocated memory	
Notes:
	

**************************************************************************/
void * smrcAllocate(StorageManager *pMgr , short shDataSize, short shNodeType, char sbData[], SMResult *psmResult)
{
	short shAllocSize = shDataSize + 3 * sizeof(short);
	AllocNode *pNode = smAlloc(pMgr, shAllocSize);
	//Check to see if memory is allocated. Returns NULL otherwise. 
	if (!pNode)
	{
		psmResult->rc = RC_NOT_AVAIL;
		return NULL;
	}
	pNode->shRefCount =1;
	pNode->shNodeType = shNodeType;
	pNode->shAllocSize = shAllocSize;
	memcpy(pNode->sbData, sbData, MAX_DATA_SZ);
	printf("%d %d",pNode->shRefCount,pNode->shNodeType);
	pNode = (AllocNode*)(((char*)pNode) + sizeof(short) * 3);
	return (pNode);

}



/******************************smrcRemoveRef*******************************************
	void *smrcRemoveRef(StorageManager *pMgr,void *pUserData , SMResult *psmResult);
Purpose:
	Removes reference to the node. Decreases reference count by 1
Parameters:
	I StorageManager	*pMgr	
	I void 			*pUserData	
	O SMResult		*psmResult
Returns:
	N/A
Notes:


***************************************************************************************/
void smrcRemoveRef(StorageManager *pMgr , void *pUserData, SMResult *psmResult)
{

	void		**pAttr;
	AllocNode	*pNewNode;
	MetaAttr	*pMetaAttr;
	
	pNewNode = (AllocNode *)(((char *)pUserData) - sizeof(short) * 3);
	pNewNode->shRefCount--;
	if (pNewNode->shRefCount == 0)
	{
		for (int iAt = pMgr->nodeTypeM[pNewNode->shNodeType].shBeginMetaAttr; pMgr->metaAttrM[iAt].shNodeType == pNewNode->shNodeType; iAt++)
		{
			pMetaAttr = &(pMgr->metaAttrM[iAt]);
			if(pMetaAttr->cDataType == 'P')
			{
				pAttr = (void **) &(pNewNode->sbData[pMetaAttr->shOffset]);
				if (!pAttr)
				{
					psmResult->rc = RC_INVALID_ADDR;
				}
				else if (*pAttr != NULL)
				{
					smrcRemoveRef(pMgr, *pAttr, psmResult);
				}
				else
				{
					printf("nothing found\n");
				}
			}
		}
		//Invoke smFree to set refcount to -1
		smFree(pMgr,pNewNode,psmResult);
	}
}



/********************************smrcAssoc*********************************************
	void *smrcAllocate(StorageManager *pMgr, short shDataSize, short shNodeType,
		 SMResult *psmResult);
Purpose:
	Accociates node to another. Increments reference count
Parameters:
	I StorageManager	*pMgr	
	I short			shDataSize
	I short			shNodeType
	O SMResult		*psmResult

Returns:
	N/A
Notes:



***************************************************************************************/
void smrcAssoc(StorageManager *pMgr , void *pUserDataFrom, char szAttrName[], void *pUserDataTo, SMResult *psmResult)
{

	void		**pAttr;
	AllocNode	*pAllocFrom;
	MetaAttr	*pMetaAttr;
	
	pAllocFrom = (AllocNode *)(((char *)pUserDataFrom) - sizeof(short)*3);
	
	for (int iAt = pMgr->nodeTypeM[pAllocFrom->shNodeType].shBeginMetaAttr; pMgr->metaAttrM[iAt].shNodeType == pAllocFrom->shNodeType; iAt++)
	{
		pMetaAttr = &(pMgr->metaAttrM[iAt]);
		if (strcmp(pMetaAttr->szAttrName, szAttrName) == 0)
		{
			if(pMetaAttr->cDataType == 'P')
			{
				pAttr = (void **)&pAllocFrom->sbData[pMetaAttr->shOffset];
				if (!pAttr)
				{
					psmResult->rc = RC_INVALID_ADDR;
				}
				else if (*pAttr != NULL)
				{
					smrcRemoveRef(pMgr, *pAttr, psmResult);
				}
				*pAttr = pUserDataTo;
				smrcAddRef(pMgr, pUserDataTo, psmResult);
			}
			else
			{
				psmResult->rc = RC_ASSOC_ATTR_NOT_PTR;
			}
		}
	}

}



/*********************************smrcAddRef*****************************************
	void *smrcAddRef(StorageManager *pMgr, void *pUserDataTo, SMResult *psmResult);
Purpose:
	Adds a reference to specified user pointer.
Parameters:
	I StorageManager	*pMgr
	I void 			*pUserDataTo	
	O SMResult		*psmResult

Returns:
	N/A
Notes:
*************************************************************************************/
void smrcAddRef(StorageManager *pMgr , void *pUserDataTo, SMResult *psmResult)
{

	AllocNode 	*pAllocTo;
	pAllocTo = (AllocNode *)(((char *)pUserDataTo) - sizeof(short) * 3);
	if (!pAllocTo)
	{
		psmResult->rc = RC_INVALID_ADDR;
	}
	pAllocTo->shRefCount++;

}

/**********************************printNode******************************************
	void printNode(StorageManager *pMgr, void *pUserData);
Purpose:
	Prints all metadata of the node that is being pointed to
Parameters:
	I StorageManager	*pMgr
	I void 			*pUserData	

Returns:
	N/A
Notes:
	
**********************************************************************************/
void printNode(StorageManager *pMgr, void *pUserData)
{
	MetaAttr 	*pMetaAttr;
	AllocNode	*pNewNode;
	char 		*pContent;

	pNewNode = (AllocNode*)(((char *)pUserData) - sizeof(short)*3);

	
	printf("%-10s %8s %15s %14s %18s\n", "Alloc Adress", "Size", "Node Type", "Ref Cnt", "Data Adress");
	printf ("%-10p %9d %9d %16d %21p\n", pNewNode, pNewNode->shAllocSize, pNewNode->shNodeType, pNewNode->shRefCount, pUserData);
	printf("%18s %-15s %s %13s\n","", "Attr Name", "Type", "Value");
	
	for (int iAt = pMgr->nodeTypeM[pNewNode->shNodeType].shBeginMetaAttr; pMgr->metaAttrM[iAt].shNodeType == pNewNode->shNodeType; iAt++)
	{
		pMetaAttr = &(pMgr->metaAttrM[iAt]);
		pContent = (char *)pUserData;
		pContent += pMetaAttr->shOffset;

		printf("%18s %-15s %-5c\t","",pMetaAttr->szAttrName, pMetaAttr->cDataType);

		if( pMetaAttr->cDataType == 'P')
		{
			printf("%-10.8LX\n" ,(long long unsigned int)*pContent);			
		}
		else if( pMetaAttr->cDataType == 'S')
		{
			printf("%-10s\n", pContent);
		}
		else if( pMetaAttr->cDataType == 'D')
		{
			//creating double ptr to typecast pContent
			double  *dContent;
			dContent = (double *) pContent;
			printf( " %-10f\n", *dContent);
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




