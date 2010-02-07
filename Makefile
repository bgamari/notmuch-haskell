OBJS = Notmuch.o Notmuch.hi NOTMUCH_H.hsc.o
LIBS = -lgmime-2.4 -lz -lnsl -lgobject-2.0 -lglib-2.0 -ltalloc -lxapian
NOTMUCHLIB = /local/src/notmuch/lib/notmuch.a

NotmuchTest.o NotmuchTest.hi: NotmuchTest.hs
	ghc --make NotmuchTest.hs

notmuchtest: $(OBJS)
	ghc --make -o notmuchtest NotmuchTest.hs $(NOTMUCHLIB) $(LIBS)
