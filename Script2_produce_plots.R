library(ggplot2)
library(ggpubr)

#######################################################################
# Plots
#######################################################################

daltonic_trick<-seq(from = 1, to = length(theta_int_fut), length.out = 10)
#pessimistic plot
P1<-ggplot()+
  geom_line(aes(theta_int_fut, P_stop_for_eff_M[,1], color ="P(early stop for efficacy)"), size=1)+
  geom_line(aes(theta_int_fut, P_not_early_stop_M[,1], color ="P(no early stop)"), size=1)+
  geom_line(aes(theta_int_fut, PoS_M[,1], color ="PoS"), size=1)+
  geom_line(aes(theta_int_fut, PoS_post_M[,1], color ="PoS_post"), size=1)+
  geom_point(aes(theta_int_fut[daltonic_trick], rep(P_stop_for_eff_M[,1],10), shape  ="P(early stop for efficacy)"), size=3)+
  geom_point(aes(theta_int_fut[daltonic_trick], P_not_early_stop_M[daltonic_trick,1], shape  ="P(no early stop)"), size=3)+
  geom_point(aes(theta_int_fut[daltonic_trick], PoS_M[daltonic_trick,1], shape  ="PoS"), size=3)+
  geom_point(aes(theta_int_fut[daltonic_trick], PoS_post_M[daltonic_trick,1], shape  ="PoS_post"), size=3)+
  {if(alpha_int!=0)
    geom_vline(aes(xintercept = theta_int_eff), linetype="dashed")}+
  ylim(c(0,1))+
  xlab(bquote(theta[fut]))+
  ylab("Probability")+
  labs(color = "", shape  = "")+
  theme_light()+
  theme(legend.title = element_text(size = 15))+
  theme(legend.key.size = unit(2, 'cm'))+
  theme(legend.text = element_text(size = 25))+
  theme(axis.text.x = element_text(size = 12))+
  theme(axis.text.y = element_text(size = 10))+
  theme(axis.title = element_text(size = 20))+
  theme(plot.title = element_text(size=22))+
  scale_x_continuous(breaks = seq(-0.4, 1, by = 0.2),
                     minor_breaks = seq(-0.4, 1, by = 0.1))+
  ggtitle("A: Pessimistic prior")

#realistic plot
P2<-ggplot()+
  geom_line(aes(theta_int_fut, P_stop_for_eff_M[,2], color ="P(early stop for efficacy)"), size=1)+
  geom_line(aes(theta_int_fut, P_not_early_stop_M[,2], color ="P(no early stop)"), size=1)+
  geom_line(aes(theta_int_fut, PoS_M[,2], color ="PoS"), size=1)+
  geom_line(aes(theta_int_fut, PoS_post_M[,2], color ="PoS_post"), size=1)+
  geom_point(aes(theta_int_fut[daltonic_trick], rep(P_stop_for_eff_M[,2],10), shape  ="P(early stop for efficacy)"), size=3)+
  geom_point(aes(theta_int_fut[daltonic_trick], P_not_early_stop_M[daltonic_trick,2], shape  ="P(no early stop)"), size=3)+
  geom_point(aes(theta_int_fut[daltonic_trick], PoS_M[daltonic_trick,2], shape  ="PoS"), size=3)+
  geom_point(aes(theta_int_fut[daltonic_trick], PoS_post_M[daltonic_trick,2], shape  ="PoS_post"), size=3)+
  {if(alpha_int!=0)
    geom_vline(aes(xintercept = theta_int_eff), linetype="dashed")}+
  ylim(c(0,1))+
  xlab(bquote(theta[fut]))+
  ylab("Probability")+
  labs(color = "", shape  = "")+
  theme_light()+
  theme(legend.title = element_text(size = 15))+
  theme(legend.text = element_text(size = 20))+
  theme(axis.text.x = element_text(size = 12))+
  theme(axis.text.y = element_text(size = 10))+
  theme(axis.title = element_text(size = 20))+
  theme(plot.title = element_text(size=22))+
  scale_x_continuous(breaks = seq(-0.4, 1, by = 0.2),
                     minor_breaks = seq(-0.4, 1, by = 0.1))+
  ggtitle("B: Realistic prior")

#optimistic plot
P3<-ggplot()+
  geom_line(aes(theta_int_fut, P_stop_for_eff_M[,3], color ="P(early stop for efficacy)"), size=1)+
  geom_line(aes(theta_int_fut, P_not_early_stop_M[,3], color ="P(no early stop)"), size=1)+
  geom_line(aes(theta_int_fut, PoS_M[,3], color ="PoS"), size=1)+
  geom_line(aes(theta_int_fut, PoS_post_M[,3], color ="PoS_post"), size=1)+
  geom_point(aes(theta_int_fut[daltonic_trick], rep(P_stop_for_eff_M[,3],10), shape  ="P(early stop for efficacy)"), size=3)+
  geom_point(aes(theta_int_fut[daltonic_trick], P_not_early_stop_M[daltonic_trick,3], shape  ="P(no early stop)"), size=3)+
  geom_point(aes(theta_int_fut[daltonic_trick], PoS_M[daltonic_trick,3], shape  ="PoS"), size=3)+
  geom_point(aes(theta_int_fut[daltonic_trick], PoS_post_M[daltonic_trick,3], shape  ="PoS_post"), size=3)+
  {if(alpha_int!=0)
    geom_vline(aes(xintercept = theta_int_eff), linetype="dashed")}+
  ylim(c(0,1))+
  xlab(bquote(theta[fut]))+
  ylab("Probability")+
  labs(color = "", shape  = "")+
  theme_light()+
  theme(legend.title = element_text(size = 15))+
  theme(legend.text = element_text(size = 20))+
  theme(axis.text.x = element_text(size = 12))+
  theme(axis.text.y = element_text(size = 10))+
  theme(axis.title = element_text(size = 20))+
  theme(plot.title = element_text(size=22))+
  scale_x_continuous(breaks = seq(-0.4, 1, by = 0.2),
                     minor_breaks = seq(-0.4, 1, by = 0.1))+
  ggtitle("C: Optimistic prior")

# See plot
#P1; P2; P3

# Put the plots together
Plegend <- get_legend(P1)

P<-ggarrange(P1+theme(legend.position = "none"),
             P2+theme(legend.position = "none"),
             P3+theme(legend.position = "none"),
             as_ggplot(Plegend),
             ncol = 2, nrow = 2)

# See plot
P
