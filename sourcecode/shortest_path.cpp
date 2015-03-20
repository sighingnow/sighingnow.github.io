//最短路径算法

# define INF 0x3fffffff		//infinity,注意保证两个INF相加不溢出

int n;			//点的数目
int map[n+1][n+1];		//若无通路，则为 INF, 每一点到自身的距离初始化为 0

void initMap();			//初始化邻接矩阵，若无通路，则初始化为 INF
 
//单源最短路 Dijkstra

void Dijkstra(int start)			//start :源点
{
	int prev[n+1];   //prev[i] 表示第 i 个点的前一个点
	int dist[n+1];		//距离
	bool isVisit[n+1];		//标记是否已经解出最短路径
	for(int i = 1; i <= n; ++i)
	{
		dist[i] = map[start][i];
		isVisit[i] = false;
		if(dist[i] == INF)
			prev[i] = 0;			//表示暂时无前一个点
		else
			prev[i] = start;
	}
	dist[start] = 0;
	isVisit[start] = true;

	for(int i = 2; i <= n; ++i)
	{
		int u = start;
		int tmp = INF;

		for(int j = 1; j <= n; ++j)
		{
			if(!isVisit[j] && dist[j] < tmp)
			{
				u = j;				//u 表示当前临接点中距离最小的点
				tmp = dist[j];
			}
		}
		isVisit[u] = true;
		//更新dist；
		for(int j = 1; j <= n; ++j)
		{
			if(!isVisit[j] && map[u][j] < INF)
			{
				int tmp = map[u][j] + d[u];
				if(tmp < d[j])
				{
					d[j] = tmp;
					prev[j] = u;
				}
			}
		}
	}
	//有dist[]易求出最短路径及相应的点，有prev[]易求出相应的路径
}


//Floyd算法，计算任意两点之间的最短路径
//输出路径算法：记录每一个点的前一个点（一般DP输出最优方案的算法）
void Floyd(void)
{
	int floydDist[n+1][n+1][n+1]; //floydDist[k][i][j]表示经过前 k 各点时 i 到 j 的距离
	for(int i = 1; i <= n; ++i)
	{
		for(int j = 1; j <= n; ++j)
		{
			floydDist[0][i][j]= map[i][j];
		}
	}
	for(int k = 1; k <= n; ++k)
	{
		for(int i = 1; i <= n; ++i)
		{
			for(int j = 1; j <= n; ++j)
			{
				floydDist[k][i][j] = floydDist[k-1][i][j] < (floydDist[k-1][i][k]+floydDist[k-1][k][j]) 
								   ? floydDist[k-1][i][j] : (floydDist[k-1][i][k]+floydDist[k-1][k][j]);
			}
		}
	}
	/*上述过程可以压缩状态空间，用二维数组实现
	Code:
	int floydDist[n+1][n+1];   //floydDist[i][j]表示点 i 到点 j 的最短距离
	for(int i = 1; i <= n; ++i)
	{
		for(int j = 1; j <= n; ++j)
		{
			floydDist[i][j]= map[i][j];
		}
	}
	for(int k = 1; k <= n; ++k)
	{
		for(int i = 1; i <= n; ++i)
		{
			for(int j = 1; j <= n; ++j)
			{
				floydDist[i][j] = floydDist[i][j] < (floydDist[i][k] + floydDist[k][j]) 
								? floydDist[i][j] : (floydDist[i][k] + floydDist[k][j]);
			}
		}
	}
	*/
}


//SPFA 算法，存在负权边且没有负权回路(Shortest Path Faster Algorithm)
/*
SPFA算法有两个优化算法 SLF 和 LLL： SLF：Small Label First 策略，设要加入的节点是j，队首元素为i，若dist(j)<dist(i)，则将j插入队
首，否则插入队尾。 LLL：Large Label Last 策略，设队首元素为i，队列中所有dist值的平均值为x，若dist(i)>x则将i插入到队尾，查找下
一元素，直到找到某一i使得dist(i)<=x，则将i出对进行松弛操作。 SLF 可使速度提高 15 ~ 20%；SLF + LLL 可提高约 50%。 在实际的应用
中SPFA的算法时间效率不是很稳定，为了避免最坏情况的出现，通常使用效率更加稳定的Dijkstra算法。
*/
//用邻接表存储图可提高效率
# include <queue>
void Spfa(int start)  //start:源点
{
	bool isVisit[n+1];
	queue <int> Q;
	int dist[n+1];
	int prev[n+1];		//prev[i] 表示第 i 个点的前一个点,用于回溯找出最优路径
	int queueTimes[n+1];
	//记录每一个点进入队列的次数，若有一个点进入队列次数大于 n ，则存在负权环，SPFA 无法解决此类问题
	for(int i = 1; i <= n; ++i)
	{
		isVisit[i] = false;
		dist[i] = INF;
		queueTimes[i] = 0;
		prev[i] = 0;
	}
	dist[start] = 0;
	isVisit[start] = true;

	Q.push(start);
	++queueTimes[start];
	while(!Q.empty())
	{
		int tmp = Q.front();
		for(int i = 1; i <= n; ++i)
		{
			if(map[tmp][i] != INF && dist[tmp] + map[tmp][i] < dist[i])
			{
				dist[i] = dist[tmp] + map[tmp][i];
				prev[i] = tmp;
				if(!isVisit[i])
				{
					Q.push(i);
					isVisit[i] = true;
					++queueTimes[i];

					if(queueTimes[i] > n)
						return;		//出现负权环
				}
			}
		}
		Q.pop();
		isVisit[tmp] = false;
	}
}//SPFA 算法也可以用邻接表存储图


//SPFA 的栈写法 Code:
void Spfa(int start)  //start:源点
{
	bool isVisit[n+1];
	int stack[n+1], top = 0;
	int dist[n+1];
	int prev[n+1];		//prev[i] 表示第 i 个点的前一个点,用于回溯找出最优路径
	int stackTimes[n+1];
	//记录每一个点进入栈的次数，若有一个点进入栈次数大于 n ，则存在负权环
	for(int i = 1; i <= n; ++i)
	{
		isVisit[i] = false;
		dist[i] = INF;
		stackTimes[i] = 0;
		prev[i] = 0;
	}
	dist[start] = 0;

	isVisit[start] = true;
	stack[top++] = start;
	++stackTimes[start];
	while(top > 0)
	{
		int tmp = stack[--top];
		for(int i = 1; i <= n; ++i)
		{
			if(map[tmp][i] != INF && dist[tmp] + map[tmp][i] < dist[i])
			{
				dist[i] = dist[tmp] + map[tmp][i];
				prev[i] = tmp;
				if(!isVisit[i])
				{
					stack[top++] = i;
					isVisit[i] = true;
					++stackTimes[i];

					if(stackTimes[i] > n)
						return;		//出现负权环
				}
			}
		}
		isVisit[tmp] = false;
	}
}
