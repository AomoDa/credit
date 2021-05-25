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
					"ed1"=10,# 高中、中专、初中及以下
					"ed2"=20,# 专科
					"ed3"=30 # 本科 、研究生
					)	
				),
			# 特长
			# hobby=list(total=5,score=5),
			# 政治背景
			political=list(
				total=15,
				# 退伍军人、党员
				score=list(
					party=5,
					veteran=10
					)
				)

			),
		# 司龄
		enter=list(
			total=50,
			# 在岗时间（月）
			score=list(
				method="logit",
				simple=1,
				transform=50
				)

			),
		# 执业认证
		qualification=list(
			total=30,
			# 获得的经纪人证和协理证，多本证书以最高级别记分
			score=list(
				"q1"=10,#杭州经纪人资格证
				"q2"=20,#全国经纪人协理证
				"q3"=30	#全国经纪人资格证
				)
			),
		# 每日一考
		exam=list(
			total=15,
			# 每日一考分
			score=list(
				"e1"=15,#96~100分
				"e2"=10, #91~95分
				"e3"=0, #80-90分
				"e4"=-10 #80分以下（不含）
				)

			)

		),
	# 行为规范
	behavior=list(
		# 日常行为规范
		daily = list(
			total=40,
			score=list(
				"d1"=-1, #迟到早退
				"d2"=-2, #会议/培训迟到早退
				"d3"=-5, #旷工（含会议/培训缺勤）
				"d4"=-10, #6S（当事人违规违纪行为）
				"d5"=-10, #交通违法（骑电瓶闯红灯）
				"d6"=-20, #参与赌博
				"d7"=-20, #聚众闹事
				"d0"=0 # 其他
				),
			# 每月回血
			recovery=2
			),
		# 行政处罚
		punish=list(
			total=60,
			score=list(
				"p1"=-10, #通报批评
				"p2"=-15, #警告处分
				"p3"=-20 #严重警告处分
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
				"c1"=-6, #一级
				"c2"=-8, #二级
				"c3"=-10,#三级
				"c4"=-15,#四级
				"c5"=-20 #五级
				),
			# 每月回血
			recovery=2	
			),

		# 客户评价
		praise=list(
			total=20,
			score=list(
				"p1"=20,#通报表扬+奖励
				"p2"=15 #表扬
				),
			cal=list(
				attenuation=6,
				coef=c(1,1,1,1,1,1)
				)
			),

		# NPS值
		nps=list(
			total=25,
			method="rank",
			score=10	,
			cal=list(
				attenuation=6,
				coef=c(1,0.6,0.4,0.2,0.2,0.1)
				),
			# 参数设置
			para=list(
				alpha=1,
				beta=8
				)

			),
		# 经验值
		jingyan=list(
			total=60,
			score=list(
				"mm12"=0.5, #买卖成交单数(二手+一手)
				"zl"=0.1, #租赁成交单
				"zl1"=0.5 #租赁一手成交单
				)
			)
		),

	# 参与贡献
	contribute=list(
		# 楼盘信息维护
		xiaoqu =list(
			total=10,
			# 得分权重
			score=5,
			# 计算方法
			cal=list(
				attenuation=6,
				coef=c(1,0.5,0.2,0.1,0.1,0.1)
				),
			# 参数设置
			para=list(
				alpha=2,
				y_alpha=0.6,
				beta=5,
				y_beta=0.9
				)

			),
		# 师徒带训
		teacher=list(
			total=30,
			score=list(
				zaizhi=5,
				zhuanzheng=8,
				limit=30
				),
			cal=list(
				attenuation=6,
				coef=c(1,1,1,0.8,0.4,0.2)				
				)
			),
		# 推荐入职
		recommend=list(
			total=20,
			score=10,
			cal=list(
				attenuation=6,
				coef=c(1,1,1,1,1,1)	
				)
			),
		# 社会活动参与
		social=list(
			total=10,
			score=10,
			cal=list(
				attenuation=6,
				coef=c(1,1,1,1,1,1)	
				)
			),
		# 其他事项
		other=list(
			total=10,
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
				total=45,
				method="rank",
				score=list(limit=10),
				cal=list(
					attenuation=6,
					coef=c(1,0.8,0.6,0.5,0.3,0.3)
					),
				# 参数设置
				para=list(
					alpha=2,
					y_alpha=0.6,
					beta=5,
					y_beta=0.9
					)
				),
			# 400接通率
			call_answer=list(
				total=10,
				method="rank",
				score=list(limit=5),
				cal=list(
					attenuation=6,
					coef=c(1,0.5,0.2,0.1,0.1,0.1)
					),
				# 参数设置
				para=list(
					alpha=3,
					beta=4
					)
				),
			# 爱聊1分钟回复
			al_1min=list(
				total=45,
				method="rank",
				score=list(limit=15),
				cal=list(
					attenuation=6,
					coef=c(1,0.8,0.5,0.3,0.2,0.2)
					),
				# 参数设置
				para=list(
					alpha=4,
					beta=5
					)
				),
			# 爱聊三日复聊率
			al_3day=list(
				total=15,
				method="rank",
				score=list(limit=5),
				cal=list(
					attenuation=6,
					coef=c(1,0.8,0.5,0.3,0.2,0.2)
					),
				# 参数设置
				para=list(
					alpha=1,
					beta=2
					)			
				),
			# 爱聊转录入率
			al_luru=list(
				total=45,
				method="rank",
				score=list(limit=15),
				cal=list(
					attenuation=6,
					coef=c(1,0.8,0.5,0.3,0.2,0.2)
					),
				# 参数设置
				para=list(
					alpha=1,
					beta=8
					)

				),
			# 爱聊转带看率
			al_daikan=list(
				total=15,
				method="rank",
				score=list(limit=5),
				cal=list(
					attenuation=6,
					coef=c(1,0.8,0.5,0.3,0.2,0.2)
					),
				# 参数设置
				para=list(
					alpha=1,
					beta=10
					)
				)


			),
		# 买卖
		mm=list(
			# 成交单数 -暂不考核
			# deal = list(
			# 	total=90,
			# 	method="transform",
			# 	score=list(
			# 		mm_2=6,
			# 		mm_1=8,
			# 		limit=90
			# 		),
			# 	cal=list(
			# 		attenuation=6,
			# 		coef=c(1,1,1,1,1,1)
			# 		),
			# 	# 参数设置
			# 	para=list(
			# 		alpha=3,
			# 		y_alpha=0.6,
			# 		beta=10,
			# 		y_beta=0.9
			# 		)
			# 	),

			# 速销(收)
			mm_s_sx = list(
				total=20,
				method="linear",
				score=list(
					limit=20,
					score=6
					),
				cal=list(
					attenuation=6,
					coef=c(1,1,1,1,1,1)
					),
				# 参数设置
				para=list(
					a=6
					)
				),
			# 速销(出)			
			mm_c_sx = list(
				total=25,
				method="linear",
				score=list(
					limit=25,
					score=8
					),
				cal=list(
					attenuation=6,
					coef=c(1,1,1,1,1,1)
					),
				# 参数设置
				para=list(
					a=8
					)
				),

			# 转介客源		
			mm_zjky = list(
				total=20,
				method="linear",
				score=list(
					limit=20,
					score=8
					),
				cal=list(
					attenuation=6,
					coef=c(1,1,1,1,1,1)
					),
				# 参数设置
				para=list(
					a=8
					)
				),

			# 折扣率
			mm_zk = list(
				total=30,
				method="grow",
				score=list(limit=10),
				cal=list(
					attenuation=6,
					coef=c(1,0.8,0.5,0.3,0.2,0.2)
					),
				# 参数设置
				para=list(
					alpha=2,
					y_alpha=0.6,
					beta=4,
					y_beta=0.9
					)
				),
			# 新增客户
			mm_cxz = list(
				total=10,
				method="grow",
				score=list(limit=5),
				cal=list(
					attenuation=6,
					coef=c(1,0.5,0.2,0.1,0.1,0.1)
					),
				# 参数设置
				para=list(
					alpha=5,
					y_alpha=0.6,
					beta=12,
					y_beta=0.9
					)
				),
			# 总带看客户组数
			mm_cdk = list(
				total=20,
				method="grow",
				score=list(limit=10),
				cal=list(
					attenuation=6,
					coef=c(1,0.5,0.2,0.1,0.1,0.1)
					),
				# 参数设置
				para=list(
					alpha=12,
					y_alpha=0.6,
					beta=24,
					y_beta=0.9
					)

				),
			# 房源新增
			mm_fxz=list(
				total=10,
				method="grow",
				score=list(limit=5),
				cal=list(
					attenuation=6,
					coef=c(1,0.5,0.2,0.1,0.1,0.1)
					),
				# 参数设置
				para=list(
					alpha=2,
					y_alpha=0.7,
					beta=5,
					y_beta=0.9
					)
				),
			# 认证委托
			mm_wt=list(
				total=20,
				method="grow",
				score=list(limit=10),
				cal=list(
					attenuation=6,
					coef=c(1,0.5,0.2,0.1,0.1,0.1)
					),
				# 参数设置
				para=list(
					alpha=2,
					y_alpha=0.7,
					beta=4,
					y_beta=0.9
					)

				),
			# 收钥匙
			mm_ys=list(
				total=10,
				method="grow",
				score=list(limit=5),
				cal=list(
					attenuation=6,
					coef=c(1,0.5,0.2,0.1,0.1,0.1)
					),
				# 参数设置
				para=list(
					alpha=1,
					y_alpha=0.7,
					beta=3,
					y_beta=0.9
					)
				)

			),
		# 租赁
		zl=list(
			# 成交单数 - 暂不考核
			# deal = list(
			# 	total=60,
			# 	method="grow",
			# 	score=list(
			# 		zl_0=1,
			# 		zl_1=8
			# 		),
			# 	cal=list(
			# 		attenuation=6,
			# 		coef=c(1,1,1,1,1,1)
			# 		),
			# 	# 参数设置
			# 	para=list(
			# 		alpha=3,
			# 		y_alpha=0.6,
			# 		beta=10,
			# 		y_beta=0.9
			# 		)
			# 	),
			# 房管收房
			zl_sfg=list(
				total=30,
				method="grow",
				score=list(limit=10),
				cal=list(
					attenuation=6,
					coef=c(1,0.8,0.5,0.3,0.2,0.2)
					),
				# 参数设置
				para=list(
					alpha=3,
					y_alpha=0.6,
					beta=6,
					y_beta=0.9
					)
				),
			# 房管出房 - 暂不考核
			# zl_cfg=list(
			# 	total=10,
			# 	method="grow",
			# 	score=5,
			# 	cal=list(
			# 		attenuation=6,
			# 		coef=c(1,0.5,0.2,0.1,0.1,0.1)
			# 		),
			# 	# 参数设置
			# 	para=list(
			# 		alpha=4,
			# 		y_alpha=0.6,
			# 		beta=6,
			# 		y_beta=0.9
			# 		)
			# 	),

			# 我收他出/他收我出
			zl_fhz=list(
				total=20,
				method="grow",
				score=list(limit=10),
				cal=list(
					attenuation=6,
					coef=c(1,0.5,0.2,0.1,0.1,0.1)
					),
				# 参数设置
				para=list(
					alpha=2,
					y_alpha=0.6,
					beta=5,
					y_beta=0.9
					)
				),

			# 出着火房管
			zl_czh=list(
				total=20,
				method="linear",
				score=list(limit=10),
				cal=list(
					attenuation=6,
					coef=c(1,0.5,0.2,0.1,0.1,0.1)
					),
				# 参数设置
				para=list(a=5)
				),


			# 租赁在管量
			zl_zg=list(
				total=30,
				score=list(limit=30),
				method="linear",
				cal=list(
					attenuation=1,
					coef=c(1)
					),	
				# 参数设置				
				para=list(a=0.4)								
				),

			# 租赁委托续签量
			zl_wt=list(
				total=15,
				score=list(limit=15),
				method="linear",
				cal=list(
					attenuation=1,
					coef=c(1)
					),					
				# 参数设置
				para=list(a=2)					
				),

			# 租赁新增客户
			zl_cxz=list(
				total=30,
				method="grow",
				score=list(limit=10),
				cal=list(
					attenuation=6,
					coef=c(1,0.8,0.5,0.3,0.2,0.2)
					),
				# 参数设置
				para=list(
					alpha=20,
					y_alpha=0.6,
					beta=30,
					y_beta=0.9
					)
				),
			# 租赁房源新增
			zl_fxz=list(
				total=20,
				method="grow",
				score=list(limit=10),
				cal=list(
					attenuation=6,
					coef=c(1,0.5,0.2,0.1,0.1,0.1)
					),
				# 参数设置
				para=list(
					alpha=3,
					y_alpha=0.6,
					beta=6,
					y_beta=0.9
					)
				),
			# 租赁普租实勘
			zl_pzsk=list(
				total=10,
				method="grow",
				score=list(limit=5),
				cal=list(
					attenuation=6,
					coef=c(1,0.5,0.2,0.1,0.1,0.1)
					),
				# 参数设置
				para=list(
					alpha=3,
					y_alpha=0.6,
					beta=6,
					y_beta=0.9
					)
				)
			)
		)
)

