env = list(
	# 基础素质
	base =list(
		# 个人背景
		jjr_backgroup = list(
			# 教育背景
			education =list(
				total=30,
				# 按学历级别计分
				score=list(
					"大专以下"=10,
					"大专非统招"=15,
					"大专统招"=20,
					"本科及以上非统招"=25,
					"本科及以上统招"=30
					)	
				),
			# 特长
			hobby=list(total=5,score=5),
			# 政治背景
			political=list(
				total=10,
				# 退伍军人、党员
				score=list(
					party=5,
					veteran=5
					)
				)

			),
		# 司龄
		enter=list(
			total=50,
			# 在岗时间（月）
			score=list(
				method="simple",
				simple=1,
				transform=50
				)

			),
		# 执业认证
		qualification=list(
			# 获得的经纪人证和协理证，多本证书以最高级别记分
			score=list(
				"杭州经纪人证"=5,
				"全国协理证"=8,
				"全国经纪人资格证"=10	
				)
			),
		# 每日一考
		exam=list(
			# 每日一考分
			score=list(
				"96~100分"=20,
				"91~95分"=10,
				"80-90分"=0,
				"80分以下（不含）"=-10
				)

			)

		),
	# 行为规范
	behavior=list(
		# 日常行为规范
		daily = list(
			total=40,
			score=list(
				"迟到早退"=-2,
				"会议/培训迟到早退"=-5,
				"旷工（含会议/培训缺勤）"=-10,
				"6S（当事人违规违纪行为）"=-10,
				"交通违法（骑电瓶闯红灯）"=-10,
				"参与赌博"=-10,
				"聚众闹事"=-20,
				"其他"=0
				),
			# 每月回血
			recovery=2
			),
		# 行政处罚
		punish=list(
			total=60,
			score=list(
				"通报批评"=-10,
				"警告处分"=-15,
				"严重警告处分"=-20
				),
			# 每月回血
			recovery=2			
			)
		),


	# 品质服务
	service=list(
		# 客户投诉
		complaint=list(
			total=80,
			score=list(
				"一级"=-5,
				"二级"=-8,
				"三级"=-10,
				"四级"=-15,
				"五级"=-20
				),
			# 每月回血
			recovery=2	
			),
		# 客户评价
		praise=list(
			total=20,
			score=list(
				"通报表扬+奖励"=20,
				"表扬"=10
				),
			cal=list(
				attenuation=6,
				coef=c(1,0.8,0.5,0.2,0.2,0.1)
				)
			),
		# NPS值
		nps=list(
			total=20,
			score=list(
				"100%"=20,
				"平均值上"=10,
				"正分值且在平均分下"=5,
				"样本量少于5个"=0,
				"负分值"=-5
				)

			),
		# 经验值
		jingyan=list(
			total=60,
			score=list(
				"买卖成交单数(二手+一手)"=0.5,
				"租赁成交单"=0.1,
				"租赁一手成交单"=0.5
				)
			)
		),

	# 参与贡献
	contribute=list(
		# 楼盘信息维护
		xiaoqu =list(
			total=10,
			# 得分权重
			score=2,
			# 计算方法
			cal=list(
				attenuation=6,
				coef=c(1,0.8,0.5,0.3,0.2,0.2)
				)
			),
		# 师徒带训
		teacher=list(
			score=list(
				zaizhi=10,
				zhuanzheng=30
				),
			cal=list(
				attenuation=6,
				coef=c(1,1,1,0.8,0.4,0.2)				
				)
			),
		# 推荐入职
		recommend=list(
			score=10,
			cal=list(
				attenuation=6,
				coef=c(1,1,1,1,1,1)	
				)
			),
		# 社会活动参与
		social=list(
			score=10,
			cal=list(
				attenuation=6,
				coef=c(1,1,1,1,1,1)	
				)
			),
		# 其他事项
		other=list(
			score=2,
			cal=list(
				attenuation=6,
				coef=c(1,1,1,1,1,1)	
				)			
			)
		),


	# 业务能力
	business=list(
		# 官网指标
		gw=list(
			# 渗透量
			app_download=list(
				total=30,
				score=list(
					per=3,
					limits=15
					),
				cal=list(
					attenuation=6,
					coef=c(1,0.8,0.5,0.3,0.2,0.2)
					)

				),
			# 400接通率
			call_answer=list(
				total=15,
				score=list(
					"91-100%"=15,
					"81-90%"=10,
					"71-80%"=8,
					"61-70%"=5,
					"60下(含)"=0
					),
				cal=list(
					attenuation=6,
					coef=c(1,0.8,0.5,0.2,0.2,0.1)
					)
				),
			# 爱聊1分钟回复
			al_1min=list(
				total=40,
				score=list(
					"91-100%"=40,
					"81-90%"=30,
					"71-80%"=20,
					"61-70%"=10,
					"60下(含)"=5,
					"0"=0
					),
				cal=list(
					attenuation=6,
					coef=c(1,0.8,0.5,0.2,0.2,0.1)
					)
				),
			# 爱聊三日复聊率
			al_3day=list(
				total=20,
				score=list(
					"51%以上"=20,
					"41-50%"=15,
					"31-40%"=10,
					"11-30%"=8,
					"10%下(含)"=5,
					"0"=0
					),
				cal=list(
					attenuation=6,
					coef=c(1,0.8,0.5,0.2,0.2,0.1)
					)				
				),
			# 爱聊转录入率
			al_luru=list(
				total=45,
				score=list(
					"31%以上"=45,
					"21-30%"=35,
					"11-20%"=25,
					"10%下(含)"=10,
					"0"=0
					),
				cal=list(
					attenuation=6,
					coef=c(1,0.8,0.5,0.2,0.2,0.1)
					)

				),
			# 爱聊转带看率
			al_daikan=list(
				total=15,
				score=list(
					"10%以上"=15,
					"7-10%"=12,
					"5-7%"=10,
					"4%下(含)"=8,
					"0"=0
					),
				cal=list(
					attenuation=6,
					coef=c(1,0.8,0.5,0.2,0.2,0.1)
					)
				)


			),
		# 买卖
		mm=list(
			# 成交单数
			deal = list(
				total=90,
				score=list(
					mm_2=6,
					mm_1=8
					),
				cal=list(
					attenuation=6,
					coef=c(1,1,1,1,1,1)
					)
				),
			# 速销
			mm_sx = list(
				total=30,
				score=6,
				cal=list(
					attenuation=6,
					coef=c(1,1,1,1,1,1)
					)
				),
			# 折扣率
			mm_zk = list(
				total=30,
				score=6,
				cal=list(
					attenuation=6,
					coef=c(1,1,1,1,1,1)
					)
				),
			# 首看客户
			mm_csk = list(
				total=10,
				score=list(
					kpi=5,
					score=6
					),
				cal=list(
					attenuation=6,
					coef=c(1,1,1,1,1,1)
					)

				),
			# 总带看客户组数
			mm_cdk = list(
				total=10,
				score=list(
					kpi=14,
					score=10
					),
				cal=list(
					attenuation=6,
					coef=c(1,1,1,1,1,1)
					)

				),
			# 房源新增
			mm_fxz=list(
				total=10,
				score=5,
				cal=list(
					attenuation=6,
					coef=c(1,1,1,1,1,1)
					)
				),
			# 认证委托
			mm_wt=list(
				total=20,
				score=5,
				cal=list(
					attenuation=6,
					coef=c(1,1,1,1,1,1)
					)

				),
			# 收钥匙
			mm_ys=list(
				total=10,
				score=5,
				cal=list(
					attenuation=6,
					coef=c(1,1,1,1,1,1)
					)

				)

			),
		# 租赁
		zl=list(
			# 成交单数
			deal = list(
				total=60,
				score=list(
					zl_0=1,
					zl_1=8
					),
				cal=list(
					attenuation=6,
					coef=c(1,1,1,1,1,1)
					)
				),
			# 房管收房
			zl_sfg=list(
				total=30,
				score=1.5,
				cal=list(
					attenuation=6,
					coef=c(1,1,1,1,1,1)
					)
				),
			# 房管出房
			zl_cfg=list(
				total=10,
				score=0.5,
				cal=list(
					attenuation=6,
					coef=c(1,1,1,1,1,1)
					)
				),
			# 租赁在管量
			zl_zg=list(
				total=40,
				score=0.4				
				),
			# 租赁委托续签量
			zl_wt=list(
				total=20,
				score=0.2	
				),
			# 租赁新增客户
			zl_cxz=list(
				total=30,
				score=1	,
				cal=list(
					attenuation=6,
					coef=c(1,0.8,0.5,0.2,0.2,0.1)
					)
				),
			# 租赁房源新增
			zl_fxz=list(
				total=10,
				score=1	,
				cal=list(
					attenuation=6,
					coef=c(1,0.8,0.5,0.2,0.2,0.1)
					)
				),
			# 租赁普租实勘
			zl_pzsk=list(
				total=10,
				score=2	,
				cal=list(
					attenuation=6,
					coef=c(1,0.8,0.5,0.2,0.2,0.1)
					)
				)
			)
		)
)

