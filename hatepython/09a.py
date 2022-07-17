# Multiple Decorators
def truncate(f):
    def inner(message):
        print(f"truncate.inner({message})")
        return f(message[:3])
    return inner


def logger(f):
    def inner(message):
        print(f"logger.inner({message})")
        print(f(message))
    return inner


@logger
@truncate
def log(message):
    import datetime
    delim = {
        'begin': '~** ',
        'end'  : ' **~'
    }
    return f"[{datetime.datetime.now()}] {delim['begin']}{message}{delim['end']}"

@truncate
@logger
def llog(message):
    import datetime
    delim = {
        'begin': '~** ',
        'end'  : ' **~'
    }
    return f"[{datetime.datetime.now()}] {delim['begin']}{message}{delim['end']}"

print(log("first"))
print(llog("first"))
