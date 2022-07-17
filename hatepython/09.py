# What we need is to pass a function in which is enhanced in some way by another function
# Decorators!!!

def noDecorator():
    def log(f, message):
        print(f(message))

    def formatter(message):
        import datetime
        delim = {
            'begin': '~** ',
            'end'  : ' **~'
        }
        return f"[{datetime.datetime.now()}] {delim['begin']}{message}{delim['end']}"

    log(formatter, "first")
    log(formatter, "second")
    log(formatter, "third")
noDecorator()

def withDecorator():
    def logger(f):
        def inner(*args):
            print(f(*args))
        return inner

    @logger
    def log(message):
        import datetime
        delim = {
            'begin': '~** ',
            'end'  : ' **~'
        }
        return f"[{datetime.datetime.now()}] {delim['begin']}{message}{delim['end']}"

    log("first")
    log("second")
    log("third")
withDecorator()