parser Parser<IH>(out IH parsedHeaders);
         package Ingress<IH>(Parser<IH> p);
         package SwitchIH(Ingress<list<void>> ingress);
