(() => {
  // output/Control.Bind/foreign.js
  var arrayBind = function(arr) {
    return function(f) {
      var result = [];
      for (var i2 = 0, l = arr.length; i2 < l; i2++) {
        Array.prototype.push.apply(result, f(arr[i2]));
      }
      return result;
    };
  };

  // output/Control.Apply/foreign.js
  var arrayApply = function(fs) {
    return function(xs) {
      var l = fs.length;
      var k = xs.length;
      var result = new Array(l * k);
      var n = 0;
      for (var i2 = 0; i2 < l; i2++) {
        var f = fs[i2];
        for (var j = 0; j < k; j++) {
          result[n++] = f(xs[j]);
        }
      }
      return result;
    };
  };

  // output/Control.Semigroupoid/index.js
  var semigroupoidFn = {
    compose: function(f) {
      return function(g) {
        return function(x) {
          return f(g(x));
        };
      };
    }
  };

  // output/Control.Category/index.js
  var identity = function(dict) {
    return dict.identity;
  };
  var categoryFn = {
    identity: function(x) {
      return x;
    },
    Semigroupoid0: function() {
      return semigroupoidFn;
    }
  };

  // output/Data.Boolean/index.js
  var otherwise = true;

  // output/Data.Function/index.js
  var flip = function(f) {
    return function(b2) {
      return function(a2) {
        return f(a2)(b2);
      };
    };
  };
  var $$const = function(a2) {
    return function(v) {
      return a2;
    };
  };

  // output/Data.Functor/foreign.js
  var arrayMap = function(f) {
    return function(arr) {
      var l = arr.length;
      var result = new Array(l);
      for (var i2 = 0; i2 < l; i2++) {
        result[i2] = f(arr[i2]);
      }
      return result;
    };
  };

  // output/Data.Unit/foreign.js
  var unit = void 0;

  // output/Type.Proxy/index.js
  var $$Proxy = /* @__PURE__ */ function() {
    function $$Proxy2() {
    }
    ;
    $$Proxy2.value = new $$Proxy2();
    return $$Proxy2;
  }();

  // output/Data.Functor/index.js
  var map = function(dict) {
    return dict.map;
  };
  var $$void = function(dictFunctor) {
    return map(dictFunctor)($$const(unit));
  };
  var voidLeft = function(dictFunctor) {
    var map115 = map(dictFunctor);
    return function(f) {
      return function(x) {
        return map115($$const(x))(f);
      };
    };
  };
  var voidRight = function(dictFunctor) {
    var map115 = map(dictFunctor);
    return function(x) {
      return map115($$const(x));
    };
  };
  var functorArray = {
    map: arrayMap
  };

  // output/Control.Apply/index.js
  var identity2 = /* @__PURE__ */ identity(categoryFn);
  var applyArray = {
    apply: arrayApply,
    Functor0: function() {
      return functorArray;
    }
  };
  var apply = function(dict) {
    return dict.apply;
  };
  var applySecond = function(dictApply) {
    var apply1 = apply(dictApply);
    var map31 = map(dictApply.Functor0());
    return function(a2) {
      return function(b2) {
        return apply1(map31($$const(identity2))(a2))(b2);
      };
    };
  };
  var lift2 = function(dictApply) {
    var apply1 = apply(dictApply);
    var map31 = map(dictApply.Functor0());
    return function(f) {
      return function(a2) {
        return function(b2) {
          return apply1(map31(f)(a2))(b2);
        };
      };
    };
  };

  // output/Control.Applicative/index.js
  var pure = function(dict) {
    return dict.pure;
  };
  var unless = function(dictApplicative) {
    var pure110 = pure(dictApplicative);
    return function(v) {
      return function(v1) {
        if (!v) {
          return v1;
        }
        ;
        if (v) {
          return pure110(unit);
        }
        ;
        throw new Error("Failed pattern match at Control.Applicative (line 68, column 1 - line 68, column 65): " + [v.constructor.name, v1.constructor.name]);
      };
    };
  };
  var when = function(dictApplicative) {
    var pure110 = pure(dictApplicative);
    return function(v) {
      return function(v1) {
        if (v) {
          return v1;
        }
        ;
        if (!v) {
          return pure110(unit);
        }
        ;
        throw new Error("Failed pattern match at Control.Applicative (line 63, column 1 - line 63, column 63): " + [v.constructor.name, v1.constructor.name]);
      };
    };
  };
  var liftA1 = function(dictApplicative) {
    var apply4 = apply(dictApplicative.Apply0());
    var pure110 = pure(dictApplicative);
    return function(f) {
      return function(a2) {
        return apply4(pure110(f))(a2);
      };
    };
  };

  // output/Control.Bind/index.js
  var discard = function(dict) {
    return dict.discard;
  };
  var bindArray = {
    bind: arrayBind,
    Apply0: function() {
      return applyArray;
    }
  };
  var bind = function(dict) {
    return dict.bind;
  };
  var bindFlipped = function(dictBind) {
    return flip(bind(dictBind));
  };
  var composeKleisliFlipped = function(dictBind) {
    var bindFlipped13 = bindFlipped(dictBind);
    return function(f) {
      return function(g) {
        return function(a2) {
          return bindFlipped13(f)(g(a2));
        };
      };
    };
  };
  var composeKleisli = function(dictBind) {
    var bind110 = bind(dictBind);
    return function(f) {
      return function(g) {
        return function(a2) {
          return bind110(f(a2))(g);
        };
      };
    };
  };
  var discardUnit = {
    discard: function(dictBind) {
      return bind(dictBind);
    }
  };

  // output/Effect.Aff/foreign.js
  var Aff = function() {
    var EMPTY = {};
    var PURE = "Pure";
    var THROW = "Throw";
    var CATCH = "Catch";
    var SYNC = "Sync";
    var ASYNC = "Async";
    var BIND = "Bind";
    var BRACKET = "Bracket";
    var FORK = "Fork";
    var SEQ = "Sequential";
    var MAP = "Map";
    var APPLY = "Apply";
    var ALT = "Alt";
    var CONS = "Cons";
    var RESUME = "Resume";
    var RELEASE = "Release";
    var FINALIZER = "Finalizer";
    var FINALIZED = "Finalized";
    var FORKED = "Forked";
    var FIBER = "Fiber";
    var THUNK = "Thunk";
    function Aff2(tag, _1, _2, _3) {
      this.tag = tag;
      this._1 = _1;
      this._2 = _2;
      this._3 = _3;
    }
    function AffCtr(tag) {
      var fn = function(_1, _2, _3) {
        return new Aff2(tag, _1, _2, _3);
      };
      fn.tag = tag;
      return fn;
    }
    function nonCanceler2(error4) {
      return new Aff2(PURE, void 0);
    }
    function runEff(eff) {
      try {
        eff();
      } catch (error4) {
        setTimeout(function() {
          throw error4;
        }, 0);
      }
    }
    function runSync(left, right, eff) {
      try {
        return right(eff());
      } catch (error4) {
        return left(error4);
      }
    }
    function runAsync(left, eff, k) {
      try {
        return eff(k)();
      } catch (error4) {
        k(left(error4))();
        return nonCanceler2;
      }
    }
    var Scheduler = function() {
      var limit = 1024;
      var size5 = 0;
      var ix = 0;
      var queue = new Array(limit);
      var draining = false;
      function drain() {
        var thunk;
        draining = true;
        while (size5 !== 0) {
          size5--;
          thunk = queue[ix];
          queue[ix] = void 0;
          ix = (ix + 1) % limit;
          thunk();
        }
        draining = false;
      }
      return {
        isDraining: function() {
          return draining;
        },
        enqueue: function(cb) {
          var i2, tmp;
          if (size5 === limit) {
            tmp = draining;
            drain();
            draining = tmp;
          }
          queue[(ix + size5) % limit] = cb;
          size5++;
          if (!draining) {
            drain();
          }
        }
      };
    }();
    function Supervisor(util) {
      var fibers = {};
      var fiberId = 0;
      var count = 0;
      return {
        register: function(fiber) {
          var fid = fiberId++;
          fiber.onComplete({
            rethrow: true,
            handler: function(result) {
              return function() {
                count--;
                delete fibers[fid];
              };
            }
          })();
          fibers[fid] = fiber;
          count++;
        },
        isEmpty: function() {
          return count === 0;
        },
        killAll: function(killError, cb) {
          return function() {
            if (count === 0) {
              return cb();
            }
            var killCount = 0;
            var kills = {};
            function kill2(fid) {
              kills[fid] = fibers[fid].kill(killError, function(result) {
                return function() {
                  delete kills[fid];
                  killCount--;
                  if (util.isLeft(result) && util.fromLeft(result)) {
                    setTimeout(function() {
                      throw util.fromLeft(result);
                    }, 0);
                  }
                  if (killCount === 0) {
                    cb();
                  }
                };
              })();
            }
            for (var k in fibers) {
              if (fibers.hasOwnProperty(k)) {
                killCount++;
                kill2(k);
              }
            }
            fibers = {};
            fiberId = 0;
            count = 0;
            return function(error4) {
              return new Aff2(SYNC, function() {
                for (var k2 in kills) {
                  if (kills.hasOwnProperty(k2)) {
                    kills[k2]();
                  }
                }
              });
            };
          };
        }
      };
    }
    var SUSPENDED = 0;
    var CONTINUE = 1;
    var STEP_BIND = 2;
    var STEP_RESULT = 3;
    var PENDING = 4;
    var RETURN = 5;
    var COMPLETED = 6;
    function Fiber(util, supervisor, aff) {
      var runTick = 0;
      var status = SUSPENDED;
      var step4 = aff;
      var fail3 = null;
      var interrupt = null;
      var bhead = null;
      var btail = null;
      var attempts = null;
      var bracketCount = 0;
      var joinId = 0;
      var joins = null;
      var rethrow = true;
      function run3(localRunTick) {
        var tmp, result, attempt;
        while (true) {
          tmp = null;
          result = null;
          attempt = null;
          switch (status) {
            case STEP_BIND:
              status = CONTINUE;
              try {
                step4 = bhead(step4);
                if (btail === null) {
                  bhead = null;
                } else {
                  bhead = btail._1;
                  btail = btail._2;
                }
              } catch (e) {
                status = RETURN;
                fail3 = util.left(e);
                step4 = null;
              }
              break;
            case STEP_RESULT:
              if (util.isLeft(step4)) {
                status = RETURN;
                fail3 = step4;
                step4 = null;
              } else if (bhead === null) {
                status = RETURN;
              } else {
                status = STEP_BIND;
                step4 = util.fromRight(step4);
              }
              break;
            case CONTINUE:
              switch (step4.tag) {
                case BIND:
                  if (bhead) {
                    btail = new Aff2(CONS, bhead, btail);
                  }
                  bhead = step4._2;
                  status = CONTINUE;
                  step4 = step4._1;
                  break;
                case PURE:
                  if (bhead === null) {
                    status = RETURN;
                    step4 = util.right(step4._1);
                  } else {
                    status = STEP_BIND;
                    step4 = step4._1;
                  }
                  break;
                case SYNC:
                  status = STEP_RESULT;
                  step4 = runSync(util.left, util.right, step4._1);
                  break;
                case ASYNC:
                  status = PENDING;
                  step4 = runAsync(util.left, step4._1, function(result2) {
                    return function() {
                      if (runTick !== localRunTick) {
                        return;
                      }
                      runTick++;
                      Scheduler.enqueue(function() {
                        if (runTick !== localRunTick + 1) {
                          return;
                        }
                        status = STEP_RESULT;
                        step4 = result2;
                        run3(runTick);
                      });
                    };
                  });
                  return;
                case THROW:
                  status = RETURN;
                  fail3 = util.left(step4._1);
                  step4 = null;
                  break;
                case CATCH:
                  if (bhead === null) {
                    attempts = new Aff2(CONS, step4, attempts, interrupt);
                  } else {
                    attempts = new Aff2(CONS, step4, new Aff2(CONS, new Aff2(RESUME, bhead, btail), attempts, interrupt), interrupt);
                  }
                  bhead = null;
                  btail = null;
                  status = CONTINUE;
                  step4 = step4._1;
                  break;
                case BRACKET:
                  bracketCount++;
                  if (bhead === null) {
                    attempts = new Aff2(CONS, step4, attempts, interrupt);
                  } else {
                    attempts = new Aff2(CONS, step4, new Aff2(CONS, new Aff2(RESUME, bhead, btail), attempts, interrupt), interrupt);
                  }
                  bhead = null;
                  btail = null;
                  status = CONTINUE;
                  step4 = step4._1;
                  break;
                case FORK:
                  status = STEP_RESULT;
                  tmp = Fiber(util, supervisor, step4._2);
                  if (supervisor) {
                    supervisor.register(tmp);
                  }
                  if (step4._1) {
                    tmp.run();
                  }
                  step4 = util.right(tmp);
                  break;
                case SEQ:
                  status = CONTINUE;
                  step4 = sequential3(util, supervisor, step4._1);
                  break;
              }
              break;
            case RETURN:
              bhead = null;
              btail = null;
              if (attempts === null) {
                status = COMPLETED;
                step4 = interrupt || fail3 || step4;
              } else {
                tmp = attempts._3;
                attempt = attempts._1;
                attempts = attempts._2;
                switch (attempt.tag) {
                  case CATCH:
                    if (interrupt && interrupt !== tmp && bracketCount === 0) {
                      status = RETURN;
                    } else if (fail3) {
                      status = CONTINUE;
                      step4 = attempt._2(util.fromLeft(fail3));
                      fail3 = null;
                    }
                    break;
                  case RESUME:
                    if (interrupt && interrupt !== tmp && bracketCount === 0 || fail3) {
                      status = RETURN;
                    } else {
                      bhead = attempt._1;
                      btail = attempt._2;
                      status = STEP_BIND;
                      step4 = util.fromRight(step4);
                    }
                    break;
                  case BRACKET:
                    bracketCount--;
                    if (fail3 === null) {
                      result = util.fromRight(step4);
                      attempts = new Aff2(CONS, new Aff2(RELEASE, attempt._2, result), attempts, tmp);
                      if (interrupt === tmp || bracketCount > 0) {
                        status = CONTINUE;
                        step4 = attempt._3(result);
                      }
                    }
                    break;
                  case RELEASE:
                    attempts = new Aff2(CONS, new Aff2(FINALIZED, step4, fail3), attempts, interrupt);
                    status = CONTINUE;
                    if (interrupt && interrupt !== tmp && bracketCount === 0) {
                      step4 = attempt._1.killed(util.fromLeft(interrupt))(attempt._2);
                    } else if (fail3) {
                      step4 = attempt._1.failed(util.fromLeft(fail3))(attempt._2);
                    } else {
                      step4 = attempt._1.completed(util.fromRight(step4))(attempt._2);
                    }
                    fail3 = null;
                    bracketCount++;
                    break;
                  case FINALIZER:
                    bracketCount++;
                    attempts = new Aff2(CONS, new Aff2(FINALIZED, step4, fail3), attempts, interrupt);
                    status = CONTINUE;
                    step4 = attempt._1;
                    break;
                  case FINALIZED:
                    bracketCount--;
                    status = RETURN;
                    step4 = attempt._1;
                    fail3 = attempt._2;
                    break;
                }
              }
              break;
            case COMPLETED:
              for (var k in joins) {
                if (joins.hasOwnProperty(k)) {
                  rethrow = rethrow && joins[k].rethrow;
                  runEff(joins[k].handler(step4));
                }
              }
              joins = null;
              if (interrupt && fail3) {
                setTimeout(function() {
                  throw util.fromLeft(fail3);
                }, 0);
              } else if (util.isLeft(step4) && rethrow) {
                setTimeout(function() {
                  if (rethrow) {
                    throw util.fromLeft(step4);
                  }
                }, 0);
              }
              return;
            case SUSPENDED:
              status = CONTINUE;
              break;
            case PENDING:
              return;
          }
        }
      }
      function onComplete(join4) {
        return function() {
          if (status === COMPLETED) {
            rethrow = rethrow && join4.rethrow;
            join4.handler(step4)();
            return function() {
            };
          }
          var jid = joinId++;
          joins = joins || {};
          joins[jid] = join4;
          return function() {
            if (joins !== null) {
              delete joins[jid];
            }
          };
        };
      }
      function kill2(error4, cb) {
        return function() {
          if (status === COMPLETED) {
            cb(util.right(void 0))();
            return function() {
            };
          }
          var canceler = onComplete({
            rethrow: false,
            handler: function() {
              return cb(util.right(void 0));
            }
          })();
          switch (status) {
            case SUSPENDED:
              interrupt = util.left(error4);
              status = COMPLETED;
              step4 = interrupt;
              run3(runTick);
              break;
            case PENDING:
              if (interrupt === null) {
                interrupt = util.left(error4);
              }
              if (bracketCount === 0) {
                if (status === PENDING) {
                  attempts = new Aff2(CONS, new Aff2(FINALIZER, step4(error4)), attempts, interrupt);
                }
                status = RETURN;
                step4 = null;
                fail3 = null;
                run3(++runTick);
              }
              break;
            default:
              if (interrupt === null) {
                interrupt = util.left(error4);
              }
              if (bracketCount === 0) {
                status = RETURN;
                step4 = null;
                fail3 = null;
              }
          }
          return canceler;
        };
      }
      function join3(cb) {
        return function() {
          var canceler = onComplete({
            rethrow: false,
            handler: cb
          })();
          if (status === SUSPENDED) {
            run3(runTick);
          }
          return canceler;
        };
      }
      return {
        kill: kill2,
        join: join3,
        onComplete,
        isSuspended: function() {
          return status === SUSPENDED;
        },
        run: function() {
          if (status === SUSPENDED) {
            if (!Scheduler.isDraining()) {
              Scheduler.enqueue(function() {
                run3(runTick);
              });
            } else {
              run3(runTick);
            }
          }
        }
      };
    }
    function runPar(util, supervisor, par, cb) {
      var fiberId = 0;
      var fibers = {};
      var killId = 0;
      var kills = {};
      var early = new Error("[ParAff] Early exit");
      var interrupt = null;
      var root = EMPTY;
      function kill2(error4, par2, cb2) {
        var step4 = par2;
        var head6 = null;
        var tail2 = null;
        var count = 0;
        var kills2 = {};
        var tmp, kid;
        loop:
          while (true) {
            tmp = null;
            switch (step4.tag) {
              case FORKED:
                if (step4._3 === EMPTY) {
                  tmp = fibers[step4._1];
                  kills2[count++] = tmp.kill(error4, function(result) {
                    return function() {
                      count--;
                      if (count === 0) {
                        cb2(result)();
                      }
                    };
                  });
                }
                if (head6 === null) {
                  break loop;
                }
                step4 = head6._2;
                if (tail2 === null) {
                  head6 = null;
                } else {
                  head6 = tail2._1;
                  tail2 = tail2._2;
                }
                break;
              case MAP:
                step4 = step4._2;
                break;
              case APPLY:
              case ALT:
                if (head6) {
                  tail2 = new Aff2(CONS, head6, tail2);
                }
                head6 = step4;
                step4 = step4._1;
                break;
            }
          }
        if (count === 0) {
          cb2(util.right(void 0))();
        } else {
          kid = 0;
          tmp = count;
          for (; kid < tmp; kid++) {
            kills2[kid] = kills2[kid]();
          }
        }
        return kills2;
      }
      function join3(result, head6, tail2) {
        var fail3, step4, lhs, rhs, tmp, kid;
        if (util.isLeft(result)) {
          fail3 = result;
          step4 = null;
        } else {
          step4 = result;
          fail3 = null;
        }
        loop:
          while (true) {
            lhs = null;
            rhs = null;
            tmp = null;
            kid = null;
            if (interrupt !== null) {
              return;
            }
            if (head6 === null) {
              cb(fail3 || step4)();
              return;
            }
            if (head6._3 !== EMPTY) {
              return;
            }
            switch (head6.tag) {
              case MAP:
                if (fail3 === null) {
                  head6._3 = util.right(head6._1(util.fromRight(step4)));
                  step4 = head6._3;
                } else {
                  head6._3 = fail3;
                }
                break;
              case APPLY:
                lhs = head6._1._3;
                rhs = head6._2._3;
                if (fail3) {
                  head6._3 = fail3;
                  tmp = true;
                  kid = killId++;
                  kills[kid] = kill2(early, fail3 === lhs ? head6._2 : head6._1, function() {
                    return function() {
                      delete kills[kid];
                      if (tmp) {
                        tmp = false;
                      } else if (tail2 === null) {
                        join3(fail3, null, null);
                      } else {
                        join3(fail3, tail2._1, tail2._2);
                      }
                    };
                  });
                  if (tmp) {
                    tmp = false;
                    return;
                  }
                } else if (lhs === EMPTY || rhs === EMPTY) {
                  return;
                } else {
                  step4 = util.right(util.fromRight(lhs)(util.fromRight(rhs)));
                  head6._3 = step4;
                }
                break;
              case ALT:
                lhs = head6._1._3;
                rhs = head6._2._3;
                if (lhs === EMPTY && util.isLeft(rhs) || rhs === EMPTY && util.isLeft(lhs)) {
                  return;
                }
                if (lhs !== EMPTY && util.isLeft(lhs) && rhs !== EMPTY && util.isLeft(rhs)) {
                  fail3 = step4 === lhs ? rhs : lhs;
                  step4 = null;
                  head6._3 = fail3;
                } else {
                  head6._3 = step4;
                  tmp = true;
                  kid = killId++;
                  kills[kid] = kill2(early, step4 === lhs ? head6._2 : head6._1, function() {
                    return function() {
                      delete kills[kid];
                      if (tmp) {
                        tmp = false;
                      } else if (tail2 === null) {
                        join3(step4, null, null);
                      } else {
                        join3(step4, tail2._1, tail2._2);
                      }
                    };
                  });
                  if (tmp) {
                    tmp = false;
                    return;
                  }
                }
                break;
            }
            if (tail2 === null) {
              head6 = null;
            } else {
              head6 = tail2._1;
              tail2 = tail2._2;
            }
          }
      }
      function resolve(fiber) {
        return function(result) {
          return function() {
            delete fibers[fiber._1];
            fiber._3 = result;
            join3(result, fiber._2._1, fiber._2._2);
          };
        };
      }
      function run3() {
        var status = CONTINUE;
        var step4 = par;
        var head6 = null;
        var tail2 = null;
        var tmp, fid;
        loop:
          while (true) {
            tmp = null;
            fid = null;
            switch (status) {
              case CONTINUE:
                switch (step4.tag) {
                  case MAP:
                    if (head6) {
                      tail2 = new Aff2(CONS, head6, tail2);
                    }
                    head6 = new Aff2(MAP, step4._1, EMPTY, EMPTY);
                    step4 = step4._2;
                    break;
                  case APPLY:
                    if (head6) {
                      tail2 = new Aff2(CONS, head6, tail2);
                    }
                    head6 = new Aff2(APPLY, EMPTY, step4._2, EMPTY);
                    step4 = step4._1;
                    break;
                  case ALT:
                    if (head6) {
                      tail2 = new Aff2(CONS, head6, tail2);
                    }
                    head6 = new Aff2(ALT, EMPTY, step4._2, EMPTY);
                    step4 = step4._1;
                    break;
                  default:
                    fid = fiberId++;
                    status = RETURN;
                    tmp = step4;
                    step4 = new Aff2(FORKED, fid, new Aff2(CONS, head6, tail2), EMPTY);
                    tmp = Fiber(util, supervisor, tmp);
                    tmp.onComplete({
                      rethrow: false,
                      handler: resolve(step4)
                    })();
                    fibers[fid] = tmp;
                    if (supervisor) {
                      supervisor.register(tmp);
                    }
                }
                break;
              case RETURN:
                if (head6 === null) {
                  break loop;
                }
                if (head6._1 === EMPTY) {
                  head6._1 = step4;
                  status = CONTINUE;
                  step4 = head6._2;
                  head6._2 = EMPTY;
                } else {
                  head6._2 = step4;
                  step4 = head6;
                  if (tail2 === null) {
                    head6 = null;
                  } else {
                    head6 = tail2._1;
                    tail2 = tail2._2;
                  }
                }
            }
          }
        root = step4;
        for (fid = 0; fid < fiberId; fid++) {
          fibers[fid].run();
        }
      }
      function cancel(error4, cb2) {
        interrupt = util.left(error4);
        var innerKills;
        for (var kid in kills) {
          if (kills.hasOwnProperty(kid)) {
            innerKills = kills[kid];
            for (kid in innerKills) {
              if (innerKills.hasOwnProperty(kid)) {
                innerKills[kid]();
              }
            }
          }
        }
        kills = null;
        var newKills = kill2(error4, root, cb2);
        return function(killError) {
          return new Aff2(ASYNC, function(killCb) {
            return function() {
              for (var kid2 in newKills) {
                if (newKills.hasOwnProperty(kid2)) {
                  newKills[kid2]();
                }
              }
              return nonCanceler2;
            };
          });
        };
      }
      run3();
      return function(killError) {
        return new Aff2(ASYNC, function(killCb) {
          return function() {
            return cancel(killError, killCb);
          };
        });
      };
    }
    function sequential3(util, supervisor, par) {
      return new Aff2(ASYNC, function(cb) {
        return function() {
          return runPar(util, supervisor, par, cb);
        };
      });
    }
    Aff2.EMPTY = EMPTY;
    Aff2.Pure = AffCtr(PURE);
    Aff2.Throw = AffCtr(THROW);
    Aff2.Catch = AffCtr(CATCH);
    Aff2.Sync = AffCtr(SYNC);
    Aff2.Async = AffCtr(ASYNC);
    Aff2.Bind = AffCtr(BIND);
    Aff2.Bracket = AffCtr(BRACKET);
    Aff2.Fork = AffCtr(FORK);
    Aff2.Seq = AffCtr(SEQ);
    Aff2.ParMap = AffCtr(MAP);
    Aff2.ParApply = AffCtr(APPLY);
    Aff2.ParAlt = AffCtr(ALT);
    Aff2.Fiber = Fiber;
    Aff2.Supervisor = Supervisor;
    Aff2.Scheduler = Scheduler;
    Aff2.nonCanceler = nonCanceler2;
    return Aff2;
  }();
  var _pure = Aff.Pure;
  var _throwError = Aff.Throw;
  function _catchError(aff) {
    return function(k) {
      return Aff.Catch(aff, k);
    };
  }
  function _map(f) {
    return function(aff) {
      if (aff.tag === Aff.Pure.tag) {
        return Aff.Pure(f(aff._1));
      } else {
        return Aff.Bind(aff, function(value15) {
          return Aff.Pure(f(value15));
        });
      }
    };
  }
  function _bind(aff) {
    return function(k) {
      return Aff.Bind(aff, k);
    };
  }
  function _fork(immediate) {
    return function(aff) {
      return Aff.Fork(immediate, aff);
    };
  }
  var _liftEffect = Aff.Sync;
  function _parAffMap(f) {
    return function(aff) {
      return Aff.ParMap(f, aff);
    };
  }
  function _parAffApply(aff1) {
    return function(aff2) {
      return Aff.ParApply(aff1, aff2);
    };
  }
  var makeAff = Aff.Async;
  function generalBracket(acquire) {
    return function(options2) {
      return function(k) {
        return Aff.Bracket(acquire, options2, k);
      };
    };
  }
  function _makeFiber(util, aff) {
    return function() {
      return Aff.Fiber(util, null, aff);
    };
  }
  var _delay = function() {
    function setDelay(n, k) {
      if (n === 0 && typeof setImmediate !== "undefined") {
        return setImmediate(k);
      } else {
        return setTimeout(k, n);
      }
    }
    function clearDelay(n, t) {
      if (n === 0 && typeof clearImmediate !== "undefined") {
        return clearImmediate(t);
      } else {
        return clearTimeout(t);
      }
    }
    return function(right, ms) {
      return Aff.Async(function(cb) {
        return function() {
          var timer2 = setDelay(ms, cb(right()));
          return function() {
            return Aff.Sync(function() {
              return right(clearDelay(ms, timer2));
            });
          };
        };
      });
    };
  }();
  var _sequential = Aff.Seq;

  // output/Control.Monad/index.js
  var unlessM = function(dictMonad) {
    var bind20 = bind(dictMonad.Bind1());
    var unless2 = unless(dictMonad.Applicative0());
    return function(mb) {
      return function(m) {
        return bind20(mb)(function(b2) {
          return unless2(b2)(m);
        });
      };
    };
  };
  var ap = function(dictMonad) {
    var bind20 = bind(dictMonad.Bind1());
    var pure21 = pure(dictMonad.Applicative0());
    return function(f) {
      return function(a2) {
        return bind20(f)(function(f$prime) {
          return bind20(a2)(function(a$prime) {
            return pure21(f$prime(a$prime));
          });
        });
      };
    };
  };

  // output/Data.Semigroup/foreign.js
  var concatString = function(s1) {
    return function(s2) {
      return s1 + s2;
    };
  };
  var concatArray = function(xs) {
    return function(ys) {
      if (xs.length === 0)
        return ys;
      if (ys.length === 0)
        return xs;
      return xs.concat(ys);
    };
  };

  // output/Data.Symbol/index.js
  var reflectSymbol = function(dict) {
    return dict.reflectSymbol;
  };

  // output/Data.Semigroup/index.js
  var semigroupUnit = {
    append: function(v) {
      return function(v1) {
        return unit;
      };
    }
  };
  var semigroupString = {
    append: concatString
  };
  var semigroupArray = {
    append: concatArray
  };
  var append = function(dict) {
    return dict.append;
  };

  // output/Control.Alt/index.js
  var alt = function(dict) {
    return dict.alt;
  };

  // output/Data.Bounded/foreign.js
  var topInt = 2147483647;
  var bottomInt = -2147483648;
  var topChar = String.fromCharCode(65535);
  var bottomChar = String.fromCharCode(0);
  var topNumber = Number.POSITIVE_INFINITY;
  var bottomNumber = Number.NEGATIVE_INFINITY;

  // output/Data.Ord/foreign.js
  var unsafeCompareImpl = function(lt) {
    return function(eq8) {
      return function(gt) {
        return function(x) {
          return function(y) {
            return x < y ? lt : x === y ? eq8 : gt;
          };
        };
      };
    };
  };
  var ordIntImpl = unsafeCompareImpl;
  var ordStringImpl = unsafeCompareImpl;
  var ordCharImpl = unsafeCompareImpl;

  // output/Data.Eq/foreign.js
  var refEq = function(r1) {
    return function(r2) {
      return r1 === r2;
    };
  };
  var eqBooleanImpl = refEq;
  var eqIntImpl = refEq;
  var eqNumberImpl = refEq;
  var eqCharImpl = refEq;
  var eqStringImpl = refEq;

  // output/Data.Eq/index.js
  var eqUnit = {
    eq: function(v) {
      return function(v1) {
        return true;
      };
    }
  };
  var eqString = {
    eq: eqStringImpl
  };
  var eqNumber = {
    eq: eqNumberImpl
  };
  var eqInt = {
    eq: eqIntImpl
  };
  var eqChar = {
    eq: eqCharImpl
  };
  var eqBoolean = {
    eq: eqBooleanImpl
  };
  var eq = function(dict) {
    return dict.eq;
  };
  var eq2 = /* @__PURE__ */ eq(eqBoolean);
  var notEq = function(dictEq) {
    var eq32 = eq(dictEq);
    return function(x) {
      return function(y) {
        return eq2(eq32(x)(y))(false);
      };
    };
  };

  // output/Data.Ordering/index.js
  var LT = /* @__PURE__ */ function() {
    function LT2() {
    }
    ;
    LT2.value = new LT2();
    return LT2;
  }();
  var GT = /* @__PURE__ */ function() {
    function GT2() {
    }
    ;
    GT2.value = new GT2();
    return GT2;
  }();
  var EQ = /* @__PURE__ */ function() {
    function EQ2() {
    }
    ;
    EQ2.value = new EQ2();
    return EQ2;
  }();

  // output/Data.Ring/foreign.js
  var intSub = function(x) {
    return function(y) {
      return x - y | 0;
    };
  };
  var numSub = function(n1) {
    return function(n2) {
      return n1 - n2;
    };
  };

  // output/Data.Semiring/foreign.js
  var intAdd = function(x) {
    return function(y) {
      return x + y | 0;
    };
  };
  var intMul = function(x) {
    return function(y) {
      return x * y | 0;
    };
  };
  var numAdd = function(n1) {
    return function(n2) {
      return n1 + n2;
    };
  };
  var numMul = function(n1) {
    return function(n2) {
      return n1 * n2;
    };
  };

  // output/Data.Semiring/index.js
  var zero = function(dict) {
    return dict.zero;
  };
  var semiringNumber = {
    add: numAdd,
    zero: 0,
    mul: numMul,
    one: 1
  };
  var semiringInt = {
    add: intAdd,
    zero: 0,
    mul: intMul,
    one: 1
  };

  // output/Data.Ring/index.js
  var sub = function(dict) {
    return dict.sub;
  };
  var ringNumber = {
    sub: numSub,
    Semiring0: function() {
      return semiringNumber;
    }
  };
  var ringInt = {
    sub: intSub,
    Semiring0: function() {
      return semiringInt;
    }
  };
  var negate = function(dictRing) {
    var sub1 = sub(dictRing);
    var zero2 = zero(dictRing.Semiring0());
    return function(a2) {
      return sub1(zero2)(a2);
    };
  };

  // output/Data.Ord/index.js
  var ordUnit = {
    compare: function(v) {
      return function(v1) {
        return EQ.value;
      };
    },
    Eq0: function() {
      return eqUnit;
    }
  };
  var ordString = /* @__PURE__ */ function() {
    return {
      compare: ordStringImpl(LT.value)(EQ.value)(GT.value),
      Eq0: function() {
        return eqString;
      }
    };
  }();
  var ordInt = /* @__PURE__ */ function() {
    return {
      compare: ordIntImpl(LT.value)(EQ.value)(GT.value),
      Eq0: function() {
        return eqInt;
      }
    };
  }();
  var ordChar = /* @__PURE__ */ function() {
    return {
      compare: ordCharImpl(LT.value)(EQ.value)(GT.value),
      Eq0: function() {
        return eqChar;
      }
    };
  }();
  var compare = function(dict) {
    return dict.compare;
  };
  var greaterThanOrEq = function(dictOrd) {
    var compare3 = compare(dictOrd);
    return function(a1) {
      return function(a2) {
        var v = compare3(a1)(a2);
        if (v instanceof LT) {
          return false;
        }
        ;
        return true;
      };
    };
  };
  var max = function(dictOrd) {
    var compare3 = compare(dictOrd);
    return function(x) {
      return function(y) {
        var v = compare3(x)(y);
        if (v instanceof LT) {
          return y;
        }
        ;
        if (v instanceof EQ) {
          return x;
        }
        ;
        if (v instanceof GT) {
          return x;
        }
        ;
        throw new Error("Failed pattern match at Data.Ord (line 181, column 3 - line 184, column 12): " + [v.constructor.name]);
      };
    };
  };
  var min = function(dictOrd) {
    var compare3 = compare(dictOrd);
    return function(x) {
      return function(y) {
        var v = compare3(x)(y);
        if (v instanceof LT) {
          return x;
        }
        ;
        if (v instanceof EQ) {
          return x;
        }
        ;
        if (v instanceof GT) {
          return y;
        }
        ;
        throw new Error("Failed pattern match at Data.Ord (line 172, column 3 - line 175, column 12): " + [v.constructor.name]);
      };
    };
  };
  var clamp = function(dictOrd) {
    var min1 = min(dictOrd);
    var max1 = max(dictOrd);
    return function(low2) {
      return function(hi) {
        return function(x) {
          return min1(hi)(max1(low2)(x));
        };
      };
    };
  };
  var abs = function(dictOrd) {
    var greaterThanOrEq1 = greaterThanOrEq(dictOrd);
    return function(dictRing) {
      var zero2 = zero(dictRing.Semiring0());
      var negate1 = negate(dictRing);
      return function(x) {
        var $99 = greaterThanOrEq1(x)(zero2);
        if ($99) {
          return x;
        }
        ;
        return negate1(x);
      };
    };
  };

  // output/Data.Bounded/index.js
  var top = function(dict) {
    return dict.top;
  };
  var boundedInt = {
    top: topInt,
    bottom: bottomInt,
    Ord0: function() {
      return ordInt;
    }
  };
  var boundedChar = {
    top: topChar,
    bottom: bottomChar,
    Ord0: function() {
      return ordChar;
    }
  };
  var bottom = function(dict) {
    return dict.bottom;
  };

  // output/Data.Show/foreign.js
  var showIntImpl = function(n) {
    return n.toString();
  };
  var showCharImpl = function(c) {
    var code2 = c.charCodeAt(0);
    if (code2 < 32 || code2 === 127) {
      switch (c) {
        case "\x07":
          return "'\\a'";
        case "\b":
          return "'\\b'";
        case "\f":
          return "'\\f'";
        case "\n":
          return "'\\n'";
        case "\r":
          return "'\\r'";
        case "	":
          return "'\\t'";
        case "\v":
          return "'\\v'";
      }
      return "'\\" + code2.toString(10) + "'";
    }
    return c === "'" || c === "\\" ? "'\\" + c + "'" : "'" + c + "'";
  };
  var showStringImpl = function(s) {
    var l = s.length;
    return '"' + s.replace(
      /[\0-\x1F\x7F"\\]/g,
      // eslint-disable-line no-control-regex
      function(c, i2) {
        switch (c) {
          case '"':
          case "\\":
            return "\\" + c;
          case "\x07":
            return "\\a";
          case "\b":
            return "\\b";
          case "\f":
            return "\\f";
          case "\n":
            return "\\n";
          case "\r":
            return "\\r";
          case "	":
            return "\\t";
          case "\v":
            return "\\v";
        }
        var k = i2 + 1;
        var empty8 = k < l && s[k] >= "0" && s[k] <= "9" ? "\\&" : "";
        return "\\" + c.charCodeAt(0).toString(10) + empty8;
      }
    ) + '"';
  };
  var showArrayImpl = function(f) {
    return function(xs) {
      var ss = [];
      for (var i2 = 0, l = xs.length; i2 < l; i2++) {
        ss[i2] = f(xs[i2]);
      }
      return "[" + ss.join(",") + "]";
    };
  };

  // output/Data.Show/index.js
  var showString = {
    show: showStringImpl
  };
  var showInt = {
    show: showIntImpl
  };
  var showChar = {
    show: showCharImpl
  };
  var show = function(dict) {
    return dict.show;
  };
  var showArray = function(dictShow) {
    return {
      show: showArrayImpl(show(dictShow))
    };
  };

  // output/Data.Generic.Rep/index.js
  var Inl = /* @__PURE__ */ function() {
    function Inl2(value0) {
      this.value0 = value0;
    }
    ;
    Inl2.create = function(value0) {
      return new Inl2(value0);
    };
    return Inl2;
  }();
  var Inr = /* @__PURE__ */ function() {
    function Inr2(value0) {
      this.value0 = value0;
    }
    ;
    Inr2.create = function(value0) {
      return new Inr2(value0);
    };
    return Inr2;
  }();
  var NoArguments = /* @__PURE__ */ function() {
    function NoArguments2() {
    }
    ;
    NoArguments2.value = new NoArguments2();
    return NoArguments2;
  }();
  var from = function(dict) {
    return dict.from;
  };

  // output/Data.Maybe/index.js
  var identity3 = /* @__PURE__ */ identity(categoryFn);
  var Nothing = /* @__PURE__ */ function() {
    function Nothing2() {
    }
    ;
    Nothing2.value = new Nothing2();
    return Nothing2;
  }();
  var Just = /* @__PURE__ */ function() {
    function Just2(value0) {
      this.value0 = value0;
    }
    ;
    Just2.create = function(value0) {
      return new Just2(value0);
    };
    return Just2;
  }();
  var maybe = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2 instanceof Nothing) {
          return v;
        }
        ;
        if (v2 instanceof Just) {
          return v1(v2.value0);
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 237, column 1 - line 237, column 51): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  };
  var isNothing = /* @__PURE__ */ maybe(true)(/* @__PURE__ */ $$const(false));
  var isJust = /* @__PURE__ */ maybe(false)(/* @__PURE__ */ $$const(true));
  var functorMaybe = {
    map: function(v) {
      return function(v1) {
        if (v1 instanceof Just) {
          return new Just(v(v1.value0));
        }
        ;
        return Nothing.value;
      };
    }
  };
  var map2 = /* @__PURE__ */ map(functorMaybe);
  var fromMaybe = function(a2) {
    return maybe(a2)(identity3);
  };
  var fromJust = function() {
    return function(v) {
      if (v instanceof Just) {
        return v.value0;
      }
      ;
      throw new Error("Failed pattern match at Data.Maybe (line 288, column 1 - line 288, column 46): " + [v.constructor.name]);
    };
  };
  var eqMaybe = function(dictEq) {
    var eq8 = eq(dictEq);
    return {
      eq: function(x) {
        return function(y) {
          if (x instanceof Nothing && y instanceof Nothing) {
            return true;
          }
          ;
          if (x instanceof Just && y instanceof Just) {
            return eq8(x.value0)(y.value0);
          }
          ;
          return false;
        };
      }
    };
  };
  var applyMaybe = {
    apply: function(v) {
      return function(v1) {
        if (v instanceof Just) {
          return map2(v.value0)(v1);
        }
        ;
        if (v instanceof Nothing) {
          return Nothing.value;
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 67, column 1 - line 69, column 30): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    Functor0: function() {
      return functorMaybe;
    }
  };
  var bindMaybe = {
    bind: function(v) {
      return function(v1) {
        if (v instanceof Just) {
          return v1(v.value0);
        }
        ;
        if (v instanceof Nothing) {
          return Nothing.value;
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 125, column 1 - line 127, column 28): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    Apply0: function() {
      return applyMaybe;
    }
  };
  var applicativeMaybe = /* @__PURE__ */ function() {
    return {
      pure: Just.create,
      Apply0: function() {
        return applyMaybe;
      }
    };
  }();
  var altMaybe = {
    alt: function(v) {
      return function(v1) {
        if (v instanceof Nothing) {
          return v1;
        }
        ;
        return v;
      };
    },
    Functor0: function() {
      return functorMaybe;
    }
  };
  var plusMaybe = /* @__PURE__ */ function() {
    return {
      empty: Nothing.value,
      Alt0: function() {
        return altMaybe;
      }
    };
  }();
  var alternativeMaybe = {
    Applicative0: function() {
      return applicativeMaybe;
    },
    Plus1: function() {
      return plusMaybe;
    }
  };

  // output/Data.Either/index.js
  var Left = /* @__PURE__ */ function() {
    function Left2(value0) {
      this.value0 = value0;
    }
    ;
    Left2.create = function(value0) {
      return new Left2(value0);
    };
    return Left2;
  }();
  var Right = /* @__PURE__ */ function() {
    function Right2(value0) {
      this.value0 = value0;
    }
    ;
    Right2.create = function(value0) {
      return new Right2(value0);
    };
    return Right2;
  }();
  var functorEither = {
    map: function(f) {
      return function(m) {
        if (m instanceof Left) {
          return new Left(m.value0);
        }
        ;
        if (m instanceof Right) {
          return new Right(f(m.value0));
        }
        ;
        throw new Error("Failed pattern match at Data.Either (line 0, column 0 - line 0, column 0): " + [m.constructor.name]);
      };
    }
  };
  var either = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2 instanceof Left) {
          return v(v2.value0);
        }
        ;
        if (v2 instanceof Right) {
          return v1(v2.value0);
        }
        ;
        throw new Error("Failed pattern match at Data.Either (line 208, column 1 - line 208, column 64): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  };

  // output/Effect/foreign.js
  var pureE = function(a2) {
    return function() {
      return a2;
    };
  };
  var bindE = function(a2) {
    return function(f) {
      return function() {
        return f(a2())();
      };
    };
  };

  // output/Data.EuclideanRing/foreign.js
  var intDegree = function(x) {
    return Math.min(Math.abs(x), 2147483647);
  };
  var intDiv = function(x) {
    return function(y) {
      if (y === 0)
        return 0;
      return y > 0 ? Math.floor(x / y) : -Math.floor(x / -y);
    };
  };
  var intMod = function(x) {
    return function(y) {
      if (y === 0)
        return 0;
      var yy = Math.abs(y);
      return (x % yy + yy) % yy;
    };
  };

  // output/Data.CommutativeRing/index.js
  var commutativeRingInt = {
    Ring0: function() {
      return ringInt;
    }
  };

  // output/Data.EuclideanRing/index.js
  var mod = function(dict) {
    return dict.mod;
  };
  var euclideanRingInt = {
    degree: intDegree,
    div: intDiv,
    mod: intMod,
    CommutativeRing0: function() {
      return commutativeRingInt;
    }
  };
  var div = function(dict) {
    return dict.div;
  };

  // output/Data.Monoid/index.js
  var monoidUnit = {
    mempty: unit,
    Semigroup0: function() {
      return semigroupUnit;
    }
  };
  var monoidString = {
    mempty: "",
    Semigroup0: function() {
      return semigroupString;
    }
  };
  var mempty = function(dict) {
    return dict.mempty;
  };

  // output/Effect/index.js
  var $runtime_lazy = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var monadEffect = {
    Applicative0: function() {
      return applicativeEffect;
    },
    Bind1: function() {
      return bindEffect;
    }
  };
  var bindEffect = {
    bind: bindE,
    Apply0: function() {
      return $lazy_applyEffect(0);
    }
  };
  var applicativeEffect = {
    pure: pureE,
    Apply0: function() {
      return $lazy_applyEffect(0);
    }
  };
  var $lazy_functorEffect = /* @__PURE__ */ $runtime_lazy("functorEffect", "Effect", function() {
    return {
      map: liftA1(applicativeEffect)
    };
  });
  var $lazy_applyEffect = /* @__PURE__ */ $runtime_lazy("applyEffect", "Effect", function() {
    return {
      apply: ap(monadEffect),
      Functor0: function() {
        return $lazy_functorEffect(0);
      }
    };
  });
  var functorEffect = /* @__PURE__ */ $lazy_functorEffect(20);
  var applyEffect = /* @__PURE__ */ $lazy_applyEffect(23);

  // output/Effect.Exception/foreign.js
  function showErrorImpl(err) {
    return err.stack || err.toString();
  }
  function error(msg) {
    return new Error(msg);
  }
  function message(e) {
    return e.message;
  }
  function throwException(e) {
    return function() {
      throw e;
    };
  }

  // output/Effect.Exception/index.js
  var $$throw = function($4) {
    return throwException(error($4));
  };
  var showError = {
    show: showErrorImpl
  };

  // output/Control.Monad.Error.Class/index.js
  var throwError = function(dict) {
    return dict.throwError;
  };
  var catchError = function(dict) {
    return dict.catchError;
  };
  var $$try = function(dictMonadError) {
    var catchError1 = catchError(dictMonadError);
    var Monad0 = dictMonadError.MonadThrow0().Monad0();
    var map31 = map(Monad0.Bind1().Apply0().Functor0());
    var pure21 = pure(Monad0.Applicative0());
    return function(a2) {
      return catchError1(map31(Right.create)(a2))(function($52) {
        return pure21(Left.create($52));
      });
    };
  };

  // output/Data.Identity/index.js
  var Identity = function(x) {
    return x;
  };
  var functorIdentity = {
    map: function(f) {
      return function(m) {
        return f(m);
      };
    }
  };
  var applyIdentity = {
    apply: function(v) {
      return function(v1) {
        return v(v1);
      };
    },
    Functor0: function() {
      return functorIdentity;
    }
  };
  var bindIdentity = {
    bind: function(v) {
      return function(f) {
        return f(v);
      };
    },
    Apply0: function() {
      return applyIdentity;
    }
  };
  var applicativeIdentity = {
    pure: Identity,
    Apply0: function() {
      return applyIdentity;
    }
  };
  var monadIdentity = {
    Applicative0: function() {
      return applicativeIdentity;
    },
    Bind1: function() {
      return bindIdentity;
    }
  };

  // output/Effect.Ref/foreign.js
  var _new = function(val) {
    return function() {
      return { value: val };
    };
  };
  var read = function(ref2) {
    return function() {
      return ref2.value;
    };
  };
  var modifyImpl = function(f) {
    return function(ref2) {
      return function() {
        var t = f(ref2.value);
        ref2.value = t.state;
        return t.value;
      };
    };
  };
  var write = function(val) {
    return function(ref2) {
      return function() {
        ref2.value = val;
      };
    };
  };

  // output/Effect.Ref/index.js
  var $$void2 = /* @__PURE__ */ $$void(functorEffect);
  var $$new = _new;
  var modify$prime = modifyImpl;
  var modify = function(f) {
    return modify$prime(function(s) {
      var s$prime = f(s);
      return {
        state: s$prime,
        value: s$prime
      };
    });
  };
  var modify_ = function(f) {
    return function(s) {
      return $$void2(modify(f)(s));
    };
  };

  // output/Control.Monad.Rec.Class/index.js
  var bindFlipped2 = /* @__PURE__ */ bindFlipped(bindEffect);
  var map3 = /* @__PURE__ */ map(functorEffect);
  var Loop = /* @__PURE__ */ function() {
    function Loop2(value0) {
      this.value0 = value0;
    }
    ;
    Loop2.create = function(value0) {
      return new Loop2(value0);
    };
    return Loop2;
  }();
  var Done = /* @__PURE__ */ function() {
    function Done2(value0) {
      this.value0 = value0;
    }
    ;
    Done2.create = function(value0) {
      return new Done2(value0);
    };
    return Done2;
  }();
  var tailRecM = function(dict) {
    return dict.tailRecM;
  };
  var tailRec = function(f) {
    var go2 = function($copy_v) {
      var $tco_done = false;
      var $tco_result;
      function $tco_loop(v) {
        if (v instanceof Loop) {
          $copy_v = f(v.value0);
          return;
        }
        ;
        if (v instanceof Done) {
          $tco_done = true;
          return v.value0;
        }
        ;
        throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 103, column 3 - line 103, column 25): " + [v.constructor.name]);
      }
      ;
      while (!$tco_done) {
        $tco_result = $tco_loop($copy_v);
      }
      ;
      return $tco_result;
    };
    return function($85) {
      return go2(f($85));
    };
  };
  var monadRecIdentity = {
    tailRecM: function(f) {
      var runIdentity = function(v) {
        return v;
      };
      var $86 = tailRec(function($88) {
        return runIdentity(f($88));
      });
      return function($87) {
        return Identity($86($87));
      };
    },
    Monad0: function() {
      return monadIdentity;
    }
  };
  var monadRecEffect = {
    tailRecM: function(f) {
      return function(a2) {
        var fromDone = function(v) {
          if (v instanceof Done) {
            return v.value0;
          }
          ;
          throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 137, column 30 - line 137, column 44): " + [v.constructor.name]);
        };
        return function __do2() {
          var r = bindFlipped2($$new)(f(a2))();
          (function() {
            while (!function __do3() {
              var v = read(r)();
              if (v instanceof Loop) {
                var e = f(v.value0)();
                write(e)(r)();
                return false;
              }
              ;
              if (v instanceof Done) {
                return true;
              }
              ;
              throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 128, column 22 - line 133, column 28): " + [v.constructor.name]);
            }()) {
            }
            ;
            return {};
          })();
          return map3(fromDone)(read(r))();
        };
      };
    },
    Monad0: function() {
      return monadEffect;
    }
  };
  var forever = function(dictMonadRec) {
    var tailRecM1 = tailRecM(dictMonadRec);
    var voidRight3 = voidRight(dictMonadRec.Monad0().Bind1().Apply0().Functor0());
    return function(ma) {
      return tailRecM1(function(u2) {
        return voidRight3(new Loop(u2))(ma);
      })(unit);
    };
  };

  // output/Unsafe.Coerce/foreign.js
  var unsafeCoerce2 = function(x) {
    return x;
  };

  // output/Control.Lazy/index.js
  var defer = function(dict) {
    return dict.defer;
  };

  // output/Data.HeytingAlgebra/foreign.js
  var boolConj = function(b1) {
    return function(b2) {
      return b1 && b2;
    };
  };
  var boolDisj = function(b1) {
    return function(b2) {
      return b1 || b2;
    };
  };
  var boolNot = function(b2) {
    return !b2;
  };

  // output/Data.HeytingAlgebra/index.js
  var tt = function(dict) {
    return dict.tt;
  };
  var not = function(dict) {
    return dict.not;
  };
  var implies = function(dict) {
    return dict.implies;
  };
  var ff = function(dict) {
    return dict.ff;
  };
  var disj = function(dict) {
    return dict.disj;
  };
  var heytingAlgebraBoolean = {
    ff: false,
    tt: true,
    implies: function(a2) {
      return function(b2) {
        return disj(heytingAlgebraBoolean)(not(heytingAlgebraBoolean)(a2))(b2);
      };
    },
    conj: boolConj,
    disj: boolDisj,
    not: boolNot
  };
  var conj = function(dict) {
    return dict.conj;
  };
  var heytingAlgebraFunction = function(dictHeytingAlgebra) {
    var ff1 = ff(dictHeytingAlgebra);
    var tt1 = tt(dictHeytingAlgebra);
    var implies1 = implies(dictHeytingAlgebra);
    var conj1 = conj(dictHeytingAlgebra);
    var disj1 = disj(dictHeytingAlgebra);
    var not1 = not(dictHeytingAlgebra);
    return {
      ff: function(v) {
        return ff1;
      },
      tt: function(v) {
        return tt1;
      },
      implies: function(f) {
        return function(g) {
          return function(a2) {
            return implies1(f(a2))(g(a2));
          };
        };
      },
      conj: function(f) {
        return function(g) {
          return function(a2) {
            return conj1(f(a2))(g(a2));
          };
        };
      },
      disj: function(f) {
        return function(g) {
          return function(a2) {
            return disj1(f(a2))(g(a2));
          };
        };
      },
      not: function(f) {
        return function(a2) {
          return not1(f(a2));
        };
      }
    };
  };

  // output/Data.Tuple/index.js
  var Tuple = /* @__PURE__ */ function() {
    function Tuple2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Tuple2.create = function(value0) {
      return function(value1) {
        return new Tuple2(value0, value1);
      };
    };
    return Tuple2;
  }();
  var snd = function(v) {
    return v.value1;
  };
  var functorTuple = {
    map: function(f) {
      return function(m) {
        return new Tuple(m.value0, f(m.value1));
      };
    }
  };
  var fst = function(v) {
    return v.value0;
  };
  var eqTuple = function(dictEq) {
    var eq8 = eq(dictEq);
    return function(dictEq1) {
      var eq12 = eq(dictEq1);
      return {
        eq: function(x) {
          return function(y) {
            return eq8(x.value0)(y.value0) && eq12(x.value1)(y.value1);
          };
        }
      };
    };
  };
  var ordTuple = function(dictOrd) {
    var compare3 = compare(dictOrd);
    var eqTuple1 = eqTuple(dictOrd.Eq0());
    return function(dictOrd1) {
      var compare12 = compare(dictOrd1);
      var eqTuple2 = eqTuple1(dictOrd1.Eq0());
      return {
        compare: function(x) {
          return function(y) {
            var v = compare3(x.value0)(y.value0);
            if (v instanceof LT) {
              return LT.value;
            }
            ;
            if (v instanceof GT) {
              return GT.value;
            }
            ;
            return compare12(x.value1)(y.value1);
          };
        },
        Eq0: function() {
          return eqTuple2;
        }
      };
    };
  };

  // output/Control.Monad.State.Class/index.js
  var state = function(dict) {
    return dict.state;
  };
  var modify_2 = function(dictMonadState) {
    var state1 = state(dictMonadState);
    return function(f) {
      return state1(function(s) {
        return new Tuple(unit, f(s));
      });
    };
  };
  var gets = function(dictMonadState) {
    var state1 = state(dictMonadState);
    return function(f) {
      return state1(function(s) {
        return new Tuple(f(s), s);
      });
    };
  };
  var get = function(dictMonadState) {
    return state(dictMonadState)(function(s) {
      return new Tuple(s, s);
    });
  };

  // output/Control.Monad.Trans.Class/index.js
  var lift = function(dict) {
    return dict.lift;
  };

  // output/Effect.Class/index.js
  var monadEffectEffect = {
    liftEffect: /* @__PURE__ */ identity(categoryFn),
    Monad0: function() {
      return monadEffect;
    }
  };
  var liftEffect = function(dict) {
    return dict.liftEffect;
  };

  // output/Control.Monad.Writer.Class/index.js
  var tell = function(dict) {
    return dict.tell;
  };

  // output/Control.Monad.Except.Trans/index.js
  var map4 = /* @__PURE__ */ map(functorEither);
  var ExceptT = function(x) {
    return x;
  };
  var runExceptT = function(v) {
    return v;
  };
  var mapExceptT = function(f) {
    return function(v) {
      return f(v);
    };
  };
  var functorExceptT = function(dictFunctor) {
    var map115 = map(dictFunctor);
    return {
      map: function(f) {
        return mapExceptT(map115(map4(f)));
      }
    };
  };
  var monadExceptT = function(dictMonad) {
    return {
      Applicative0: function() {
        return applicativeExceptT(dictMonad);
      },
      Bind1: function() {
        return bindExceptT(dictMonad);
      }
    };
  };
  var bindExceptT = function(dictMonad) {
    var bind20 = bind(dictMonad.Bind1());
    var pure21 = pure(dictMonad.Applicative0());
    return {
      bind: function(v) {
        return function(k) {
          return bind20(v)(either(function($187) {
            return pure21(Left.create($187));
          })(function(a2) {
            var v1 = k(a2);
            return v1;
          }));
        };
      },
      Apply0: function() {
        return applyExceptT(dictMonad);
      }
    };
  };
  var applyExceptT = function(dictMonad) {
    var functorExceptT1 = functorExceptT(dictMonad.Bind1().Apply0().Functor0());
    return {
      apply: ap(monadExceptT(dictMonad)),
      Functor0: function() {
        return functorExceptT1;
      }
    };
  };
  var applicativeExceptT = function(dictMonad) {
    return {
      pure: function() {
        var $188 = pure(dictMonad.Applicative0());
        return function($189) {
          return ExceptT($188(Right.create($189)));
        };
      }(),
      Apply0: function() {
        return applyExceptT(dictMonad);
      }
    };
  };
  var monadThrowExceptT = function(dictMonad) {
    var monadExceptT1 = monadExceptT(dictMonad);
    return {
      throwError: function() {
        var $198 = pure(dictMonad.Applicative0());
        return function($199) {
          return ExceptT($198(Left.create($199)));
        };
      }(),
      Monad0: function() {
        return monadExceptT1;
      }
    };
  };
  var altExceptT = function(dictSemigroup) {
    var append9 = append(dictSemigroup);
    return function(dictMonad) {
      var Bind1 = dictMonad.Bind1();
      var bind20 = bind(Bind1);
      var pure21 = pure(dictMonad.Applicative0());
      var functorExceptT1 = functorExceptT(Bind1.Apply0().Functor0());
      return {
        alt: function(v) {
          return function(v1) {
            return bind20(v)(function(rm) {
              if (rm instanceof Right) {
                return pure21(new Right(rm.value0));
              }
              ;
              if (rm instanceof Left) {
                return bind20(v1)(function(rn) {
                  if (rn instanceof Right) {
                    return pure21(new Right(rn.value0));
                  }
                  ;
                  if (rn instanceof Left) {
                    return pure21(new Left(append9(rm.value0)(rn.value0)));
                  }
                  ;
                  throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 86, column 9 - line 88, column 49): " + [rn.constructor.name]);
                });
              }
              ;
              throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 82, column 5 - line 88, column 49): " + [rm.constructor.name]);
            });
          };
        },
        Functor0: function() {
          return functorExceptT1;
        }
      };
    };
  };

  // output/Control.Plus/index.js
  var empty = function(dict) {
    return dict.empty;
  };

  // output/Safe.Coerce/index.js
  var coerce = function() {
    return unsafeCoerce2;
  };

  // output/Data.Newtype/index.js
  var coerce2 = /* @__PURE__ */ coerce();
  var unwrap = function() {
    return coerce2;
  };
  var over = function() {
    return function() {
      return function(v) {
        return coerce2;
      };
    };
  };

  // output/Control.Monad.Writer.Trans/index.js
  var WriterT = function(x) {
    return x;
  };
  var monadTransWriterT = function(dictMonoid) {
    var mempty6 = mempty(dictMonoid);
    return {
      lift: function(dictMonad) {
        var bind20 = bind(dictMonad.Bind1());
        var pure21 = pure(dictMonad.Applicative0());
        return function(m) {
          return bind20(m)(function(a2) {
            return pure21(new Tuple(a2, mempty6));
          });
        };
      }
    };
  };
  var mapWriterT = function(f) {
    return function(v) {
      return f(v);
    };
  };
  var functorWriterT = function(dictFunctor) {
    var map31 = map(dictFunctor);
    return {
      map: function(f) {
        return mapWriterT(map31(function(v) {
          return new Tuple(f(v.value0), v.value1);
        }));
      }
    };
  };
  var execWriterT = function(dictFunctor) {
    var map31 = map(dictFunctor);
    return function(v) {
      return map31(snd)(v);
    };
  };
  var applyWriterT = function(dictSemigroup) {
    var append9 = append(dictSemigroup);
    return function(dictApply) {
      var apply4 = apply(dictApply);
      var Functor0 = dictApply.Functor0();
      var map31 = map(Functor0);
      var functorWriterT1 = functorWriterT(Functor0);
      return {
        apply: function(v) {
          return function(v1) {
            var k = function(v3) {
              return function(v4) {
                return new Tuple(v3.value0(v4.value0), append9(v3.value1)(v4.value1));
              };
            };
            return apply4(map31(k)(v))(v1);
          };
        },
        Functor0: function() {
          return functorWriterT1;
        }
      };
    };
  };
  var bindWriterT = function(dictSemigroup) {
    var append9 = append(dictSemigroup);
    var applyWriterT1 = applyWriterT(dictSemigroup);
    return function(dictBind) {
      var bind20 = bind(dictBind);
      var Apply0 = dictBind.Apply0();
      var map31 = map(Apply0.Functor0());
      var applyWriterT2 = applyWriterT1(Apply0);
      return {
        bind: function(v) {
          return function(k) {
            return bind20(v)(function(v1) {
              var v2 = k(v1.value0);
              return map31(function(v3) {
                return new Tuple(v3.value0, append9(v1.value1)(v3.value1));
              })(v2);
            });
          };
        },
        Apply0: function() {
          return applyWriterT2;
        }
      };
    };
  };
  var semigroupWriterT = function(dictApply) {
    return function(dictSemigroup) {
      var lift23 = lift2(applyWriterT(dictSemigroup)(dictApply));
      return function(dictSemigroup1) {
        return {
          append: lift23(append(dictSemigroup1))
        };
      };
    };
  };
  var applicativeWriterT = function(dictMonoid) {
    var mempty6 = mempty(dictMonoid);
    var applyWriterT1 = applyWriterT(dictMonoid.Semigroup0());
    return function(dictApplicative) {
      var pure21 = pure(dictApplicative);
      var applyWriterT2 = applyWriterT1(dictApplicative.Apply0());
      return {
        pure: function(a2) {
          return pure21(new Tuple(a2, mempty6));
        },
        Apply0: function() {
          return applyWriterT2;
        }
      };
    };
  };
  var monadWriterT = function(dictMonoid) {
    var applicativeWriterT1 = applicativeWriterT(dictMonoid);
    var bindWriterT1 = bindWriterT(dictMonoid.Semigroup0());
    return function(dictMonad) {
      var applicativeWriterT2 = applicativeWriterT1(dictMonad.Applicative0());
      var bindWriterT22 = bindWriterT1(dictMonad.Bind1());
      return {
        Applicative0: function() {
          return applicativeWriterT2;
        },
        Bind1: function() {
          return bindWriterT22;
        }
      };
    };
  };
  var monadEffectWriter = function(dictMonoid) {
    var lift4 = lift(monadTransWriterT(dictMonoid));
    var monadWriterT1 = monadWriterT(dictMonoid);
    return function(dictMonadEffect) {
      var Monad0 = dictMonadEffect.Monad0();
      var monadWriterT2 = monadWriterT1(Monad0);
      return {
        liftEffect: function() {
          var $249 = lift4(Monad0);
          var $250 = liftEffect(dictMonadEffect);
          return function($251) {
            return $249($250($251));
          };
        }(),
        Monad0: function() {
          return monadWriterT2;
        }
      };
    };
  };
  var monadTellWriterT = function(dictMonoid) {
    var Semigroup0 = dictMonoid.Semigroup0();
    var monadWriterT1 = monadWriterT(dictMonoid);
    return function(dictMonad) {
      var monadWriterT2 = monadWriterT1(dictMonad);
      return {
        tell: function() {
          var $252 = pure(dictMonad.Applicative0());
          var $253 = Tuple.create(unit);
          return function($254) {
            return WriterT($252($253($254)));
          };
        }(),
        Semigroup0: function() {
          return Semigroup0;
        },
        Monad1: function() {
          return monadWriterT2;
        }
      };
    };
  };
  var monoidWriterT = function(dictApplicative) {
    var semigroupWriterT1 = semigroupWriterT(dictApplicative.Apply0());
    return function(dictMonoid) {
      var pure21 = pure(applicativeWriterT(dictMonoid)(dictApplicative));
      var semigroupWriterT2 = semigroupWriterT1(dictMonoid.Semigroup0());
      return function(dictMonoid1) {
        var semigroupWriterT3 = semigroupWriterT2(dictMonoid1.Semigroup0());
        return {
          mempty: pure21(mempty(dictMonoid1)),
          Semigroup0: function() {
            return semigroupWriterT3;
          }
        };
      };
    };
  };

  // output/Control.Parallel.Class/index.js
  var sequential = function(dict) {
    return dict.sequential;
  };
  var parallel = function(dict) {
    return dict.parallel;
  };

  // output/Data.Foldable/foreign.js
  var foldrArray = function(f) {
    return function(init3) {
      return function(xs) {
        var acc = init3;
        var len = xs.length;
        for (var i2 = len - 1; i2 >= 0; i2--) {
          acc = f(xs[i2])(acc);
        }
        return acc;
      };
    };
  };
  var foldlArray = function(f) {
    return function(init3) {
      return function(xs) {
        var acc = init3;
        var len = xs.length;
        for (var i2 = 0; i2 < len; i2++) {
          acc = f(acc)(xs[i2]);
        }
        return acc;
      };
    };
  };

  // output/Data.Bifunctor/index.js
  var bimap = function(dict) {
    return dict.bimap;
  };

  // output/Data.Foldable/index.js
  var foldr = function(dict) {
    return dict.foldr;
  };
  var traverse_ = function(dictApplicative) {
    var applySecond4 = applySecond(dictApplicative.Apply0());
    var pure21 = pure(dictApplicative);
    return function(dictFoldable) {
      var foldr22 = foldr(dictFoldable);
      return function(f) {
        return foldr22(function($454) {
          return applySecond4(f($454));
        })(pure21(unit));
      };
    };
  };
  var for_ = function(dictApplicative) {
    var traverse_14 = traverse_(dictApplicative);
    return function(dictFoldable) {
      return flip(traverse_14(dictFoldable));
    };
  };
  var foldl = function(dict) {
    return dict.foldl;
  };
  var foldableMaybe = {
    foldr: function(v) {
      return function(v1) {
        return function(v2) {
          if (v2 instanceof Nothing) {
            return v1;
          }
          ;
          if (v2 instanceof Just) {
            return v(v2.value0)(v1);
          }
          ;
          throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
        };
      };
    },
    foldl: function(v) {
      return function(v1) {
        return function(v2) {
          if (v2 instanceof Nothing) {
            return v1;
          }
          ;
          if (v2 instanceof Just) {
            return v(v1)(v2.value0);
          }
          ;
          throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
        };
      };
    },
    foldMap: function(dictMonoid) {
      var mempty6 = mempty(dictMonoid);
      return function(v) {
        return function(v1) {
          if (v1 instanceof Nothing) {
            return mempty6;
          }
          ;
          if (v1 instanceof Just) {
            return v(v1.value0);
          }
          ;
          throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [v.constructor.name, v1.constructor.name]);
        };
      };
    }
  };
  var foldMapDefaultR = function(dictFoldable) {
    var foldr22 = foldr(dictFoldable);
    return function(dictMonoid) {
      var append9 = append(dictMonoid.Semigroup0());
      var mempty6 = mempty(dictMonoid);
      return function(f) {
        return foldr22(function(x) {
          return function(acc) {
            return append9(f(x))(acc);
          };
        })(mempty6);
      };
    };
  };
  var foldableArray = {
    foldr: foldrArray,
    foldl: foldlArray,
    foldMap: function(dictMonoid) {
      return foldMapDefaultR(foldableArray)(dictMonoid);
    }
  };
  var foldMap = function(dict) {
    return dict.foldMap;
  };
  var foldM = function(dictFoldable) {
    var foldl22 = foldl(dictFoldable);
    return function(dictMonad) {
      var bind20 = bind(dictMonad.Bind1());
      var pure21 = pure(dictMonad.Applicative0());
      return function(f) {
        return function(b0) {
          return foldl22(function(b2) {
            return function(a2) {
              return bind20(b2)(flip(f)(a2));
            };
          })(pure21(b0));
        };
      };
    };
  };

  // output/Data.Traversable/foreign.js
  var traverseArrayImpl = function() {
    function array1(a2) {
      return [a2];
    }
    function array2(a2) {
      return function(b2) {
        return [a2, b2];
      };
    }
    function array3(a2) {
      return function(b2) {
        return function(c) {
          return [a2, b2, c];
        };
      };
    }
    function concat2(xs) {
      return function(ys) {
        return xs.concat(ys);
      };
    }
    return function(apply4) {
      return function(map31) {
        return function(pure21) {
          return function(f) {
            return function(array) {
              function go2(bot, top3) {
                switch (top3 - bot) {
                  case 0:
                    return pure21([]);
                  case 1:
                    return map31(array1)(f(array[bot]));
                  case 2:
                    return apply4(map31(array2)(f(array[bot])))(f(array[bot + 1]));
                  case 3:
                    return apply4(apply4(map31(array3)(f(array[bot])))(f(array[bot + 1])))(f(array[bot + 2]));
                  default:
                    var pivot = bot + Math.floor((top3 - bot) / 4) * 2;
                    return apply4(map31(concat2)(go2(bot, pivot)))(go2(pivot, top3));
                }
              }
              return go2(0, array.length);
            };
          };
        };
      };
    };
  }();

  // output/Data.Traversable/index.js
  var identity4 = /* @__PURE__ */ identity(categoryFn);
  var traverse = function(dict) {
    return dict.traverse;
  };
  var sequenceDefault = function(dictTraversable) {
    var traverse2 = traverse(dictTraversable);
    return function(dictApplicative) {
      return traverse2(dictApplicative)(identity4);
    };
  };
  var traversableArray = {
    traverse: function(dictApplicative) {
      var Apply0 = dictApplicative.Apply0();
      return traverseArrayImpl(apply(Apply0))(map(Apply0.Functor0()))(pure(dictApplicative));
    },
    sequence: function(dictApplicative) {
      return sequenceDefault(traversableArray)(dictApplicative);
    },
    Functor0: function() {
      return functorArray;
    },
    Foldable1: function() {
      return foldableArray;
    }
  };
  var sequence = function(dict) {
    return dict.sequence;
  };

  // output/Control.Parallel/index.js
  var identity5 = /* @__PURE__ */ identity(categoryFn);
  var parTraverse_ = function(dictParallel) {
    var sequential3 = sequential(dictParallel);
    var traverse_8 = traverse_(dictParallel.Applicative1());
    var parallel3 = parallel(dictParallel);
    return function(dictFoldable) {
      var traverse_14 = traverse_8(dictFoldable);
      return function(f) {
        var $48 = traverse_14(function($50) {
          return parallel3(f($50));
        });
        return function($49) {
          return sequential3($48($49));
        };
      };
    };
  };
  var parSequence_ = function(dictParallel) {
    var parTraverse_1 = parTraverse_(dictParallel);
    return function(dictFoldable) {
      return parTraverse_1(dictFoldable)(identity5);
    };
  };

  // output/Data.Time.Duration/index.js
  var over2 = /* @__PURE__ */ over()();
  var negate2 = /* @__PURE__ */ negate(ringNumber);
  var identity6 = /* @__PURE__ */ identity(categoryFn);
  var Minutes = function(x) {
    return x;
  };
  var Milliseconds = function(x) {
    return x;
  };
  var toDuration = function(dict) {
    return dict.toDuration;
  };
  var semigroupMilliseconds = {
    append: function(v) {
      return function(v1) {
        return v + v1;
      };
    }
  };
  var fromDuration = function(dict) {
    return dict.fromDuration;
  };
  var negateDuration = function(dictDuration) {
    var $57 = toDuration(dictDuration);
    var $58 = over2(Milliseconds)(negate2);
    var $59 = fromDuration(dictDuration);
    return function($60) {
      return $57($58($59($60)));
    };
  };
  var eqMilliseconds = eqNumber;
  var durationMinutes = {
    fromDuration: /* @__PURE__ */ over2(Minutes)(function(v) {
      return v * 6e4;
    }),
    toDuration: /* @__PURE__ */ over2(Milliseconds)(function(v) {
      return v / 6e4;
    })
  };
  var durationMilliseconds = {
    fromDuration: identity6,
    toDuration: identity6
  };

  // output/Effect.Unsafe/foreign.js
  var unsafePerformEffect = function(f) {
    return f();
  };

  // output/Partial.Unsafe/foreign.js
  var _unsafePartial = function(f) {
    return f();
  };

  // output/Partial/foreign.js
  var _crashWith = function(msg) {
    throw new Error(msg);
  };

  // output/Partial/index.js
  var crashWith = function() {
    return _crashWith;
  };

  // output/Partial.Unsafe/index.js
  var crashWith2 = /* @__PURE__ */ crashWith();
  var unsafePartial = _unsafePartial;
  var unsafeCrashWith = function(msg) {
    return unsafePartial(function() {
      return crashWith2(msg);
    });
  };

  // output/Effect.Aff/index.js
  var $runtime_lazy2 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var pure2 = /* @__PURE__ */ pure(applicativeEffect);
  var $$void3 = /* @__PURE__ */ $$void(functorEffect);
  var map5 = /* @__PURE__ */ map(functorEffect);
  var Canceler = function(x) {
    return x;
  };
  var suspendAff = /* @__PURE__ */ _fork(false);
  var functorParAff = {
    map: _parAffMap
  };
  var functorAff = {
    map: _map
  };
  var map1 = /* @__PURE__ */ map(functorAff);
  var forkAff = /* @__PURE__ */ _fork(true);
  var ffiUtil = /* @__PURE__ */ function() {
    var unsafeFromRight = function(v) {
      if (v instanceof Right) {
        return v.value0;
      }
      ;
      if (v instanceof Left) {
        return unsafeCrashWith("unsafeFromRight: Left");
      }
      ;
      throw new Error("Failed pattern match at Effect.Aff (line 412, column 21 - line 414, column 54): " + [v.constructor.name]);
    };
    var unsafeFromLeft = function(v) {
      if (v instanceof Left) {
        return v.value0;
      }
      ;
      if (v instanceof Right) {
        return unsafeCrashWith("unsafeFromLeft: Right");
      }
      ;
      throw new Error("Failed pattern match at Effect.Aff (line 407, column 20 - line 409, column 55): " + [v.constructor.name]);
    };
    var isLeft = function(v) {
      if (v instanceof Left) {
        return true;
      }
      ;
      if (v instanceof Right) {
        return false;
      }
      ;
      throw new Error("Failed pattern match at Effect.Aff (line 402, column 12 - line 404, column 21): " + [v.constructor.name]);
    };
    return {
      isLeft,
      fromLeft: unsafeFromLeft,
      fromRight: unsafeFromRight,
      left: Left.create,
      right: Right.create
    };
  }();
  var makeFiber = function(aff) {
    return _makeFiber(ffiUtil, aff);
  };
  var launchAff = function(aff) {
    return function __do2() {
      var fiber = makeFiber(aff)();
      fiber.run();
      return fiber;
    };
  };
  var launchAff_ = function($74) {
    return $$void3(launchAff($74));
  };
  var delay = function(v) {
    return _delay(Right.create, v);
  };
  var bracket = function(acquire) {
    return function(completed) {
      return generalBracket(acquire)({
        killed: $$const(completed),
        failed: $$const(completed),
        completed: $$const(completed)
      });
    };
  };
  var applyParAff = {
    apply: _parAffApply,
    Functor0: function() {
      return functorParAff;
    }
  };
  var monadAff = {
    Applicative0: function() {
      return applicativeAff;
    },
    Bind1: function() {
      return bindAff;
    }
  };
  var bindAff = {
    bind: _bind,
    Apply0: function() {
      return $lazy_applyAff(0);
    }
  };
  var applicativeAff = {
    pure: _pure,
    Apply0: function() {
      return $lazy_applyAff(0);
    }
  };
  var $lazy_applyAff = /* @__PURE__ */ $runtime_lazy2("applyAff", "Effect.Aff", function() {
    return {
      apply: ap(monadAff),
      Functor0: function() {
        return functorAff;
      }
    };
  });
  var pure22 = /* @__PURE__ */ pure(applicativeAff);
  var bind1 = /* @__PURE__ */ bind(bindAff);
  var bindFlipped3 = /* @__PURE__ */ bindFlipped(bindAff);
  var $$finally = function(fin) {
    return function(a2) {
      return bracket(pure22(unit))($$const(fin))($$const(a2));
    };
  };
  var monadEffectAff = {
    liftEffect: _liftEffect,
    Monad0: function() {
      return monadAff;
    }
  };
  var liftEffect2 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var effectCanceler = function($75) {
    return Canceler($$const(liftEffect2($75)));
  };
  var joinFiber = function(v) {
    return makeAff(function(k) {
      return map5(effectCanceler)(v.join(k));
    });
  };
  var functorFiber = {
    map: function(f) {
      return function(t) {
        return unsafePerformEffect(makeFiber(map1(f)(joinFiber(t))));
      };
    }
  };
  var killFiber = function(e) {
    return function(v) {
      return bind1(liftEffect2(v.isSuspended))(function(suspended) {
        if (suspended) {
          return liftEffect2($$void3(v.kill(e, $$const(pure2(unit)))));
        }
        ;
        return makeAff(function(k) {
          return map5(effectCanceler)(v.kill(e, k));
        });
      });
    };
  };
  var monadThrowAff = {
    throwError: _throwError,
    Monad0: function() {
      return monadAff;
    }
  };
  var monadErrorAff = {
    catchError: _catchError,
    MonadThrow0: function() {
      return monadThrowAff;
    }
  };
  var $$try2 = /* @__PURE__ */ $$try(monadErrorAff);
  var runAff = function(k) {
    return function(aff) {
      return launchAff(bindFlipped3(function($80) {
        return liftEffect2(k($80));
      })($$try2(aff)));
    };
  };
  var runAff_ = function(k) {
    return function(aff) {
      return $$void3(runAff(k)(aff));
    };
  };
  var parallelAff = {
    parallel: unsafeCoerce2,
    sequential: _sequential,
    Monad0: function() {
      return monadAff;
    },
    Applicative1: function() {
      return $lazy_applicativeParAff(0);
    }
  };
  var $lazy_applicativeParAff = /* @__PURE__ */ $runtime_lazy2("applicativeParAff", "Effect.Aff", function() {
    return {
      pure: function() {
        var $82 = parallel(parallelAff);
        return function($83) {
          return $82(pure22($83));
        };
      }(),
      Apply0: function() {
        return applyParAff;
      }
    };
  });
  var applicativeParAff = /* @__PURE__ */ $lazy_applicativeParAff(136);
  var parSequence_2 = /* @__PURE__ */ parSequence_(parallelAff)(foldableArray);
  var semigroupCanceler = {
    append: function(v) {
      return function(v1) {
        return function(err) {
          return parSequence_2([v(err), v1(err)]);
        };
      };
    }
  };
  var monadRecAff = {
    tailRecM: function(k) {
      var go2 = function(a2) {
        return bind1(k(a2))(function(res) {
          if (res instanceof Done) {
            return pure22(res.value0);
          }
          ;
          if (res instanceof Loop) {
            return go2(res.value0);
          }
          ;
          throw new Error("Failed pattern match at Effect.Aff (line 104, column 7 - line 106, column 23): " + [res.constructor.name]);
        });
      };
      return go2;
    },
    Monad0: function() {
      return monadAff;
    }
  };
  var nonCanceler = /* @__PURE__ */ $$const(/* @__PURE__ */ pure22(unit));
  var monoidCanceler = {
    mempty: nonCanceler,
    Semigroup0: function() {
      return semigroupCanceler;
    }
  };

  // output/Data.Lazy/foreign.js
  var defer2 = function(thunk) {
    var v = null;
    return function() {
      if (thunk === void 0)
        return v;
      v = thunk();
      thunk = void 0;
      return v;
    };
  };
  var force = function(l) {
    return l();
  };

  // output/Effect.Aff.Class/index.js
  var monadAffAff = {
    liftAff: /* @__PURE__ */ identity(categoryFn),
    MonadEffect0: function() {
      return monadEffectAff;
    }
  };
  var liftAff = function(dict) {
    return dict.liftAff;
  };

  // output/Web.DOM.ParentNode/foreign.js
  var getEffProp = function(name15) {
    return function(node) {
      return function() {
        return node[name15];
      };
    };
  };
  var children = getEffProp("children");
  var _firstElementChild = getEffProp("firstElementChild");
  var _lastElementChild = getEffProp("lastElementChild");
  var childElementCount = getEffProp("childElementCount");
  function _querySelector(selector) {
    return function(node) {
      return function() {
        return node.querySelector(selector);
      };
    };
  }

  // output/Data.Nullable/foreign.js
  var nullImpl = null;
  function nullable(a2, r, f) {
    return a2 == null ? r : f(a2);
  }
  function notNull(x) {
    return x;
  }

  // output/Data.Nullable/index.js
  var toNullable = /* @__PURE__ */ maybe(nullImpl)(notNull);
  var toMaybe = function(n) {
    return nullable(n, Nothing.value, Just.create);
  };

  // output/Web.DOM.ParentNode/index.js
  var map6 = /* @__PURE__ */ map(functorEffect);
  var querySelector = function(qs) {
    var $2 = map6(toMaybe);
    var $3 = _querySelector(qs);
    return function($4) {
      return $2($3($4));
    };
  };

  // output/Web.Event.EventTarget/foreign.js
  function eventListener(fn) {
    return function() {
      return function(event) {
        return fn(event)();
      };
    };
  }
  function addEventListener(type) {
    return function(listener) {
      return function(useCapture) {
        return function(target7) {
          return function() {
            return target7.addEventListener(type, listener, useCapture);
          };
        };
      };
    };
  }
  function removeEventListener(type) {
    return function(listener) {
      return function(useCapture) {
        return function(target7) {
          return function() {
            return target7.removeEventListener(type, listener, useCapture);
          };
        };
      };
    };
  }

  // output/Web.HTML/foreign.js
  var windowImpl = function() {
    return window;
  };

  // output/Web.HTML.HTMLDocument/foreign.js
  function _readyState(doc) {
    return doc.readyState;
  }

  // output/Web.HTML.HTMLDocument.ReadyState/index.js
  var Loading = /* @__PURE__ */ function() {
    function Loading2() {
    }
    ;
    Loading2.value = new Loading2();
    return Loading2;
  }();
  var Interactive = /* @__PURE__ */ function() {
    function Interactive2() {
    }
    ;
    Interactive2.value = new Interactive2();
    return Interactive2;
  }();
  var Complete = /* @__PURE__ */ function() {
    function Complete2() {
    }
    ;
    Complete2.value = new Complete2();
    return Complete2;
  }();
  var parse = function(v) {
    if (v === "loading") {
      return new Just(Loading.value);
    }
    ;
    if (v === "interactive") {
      return new Just(Interactive.value);
    }
    ;
    if (v === "complete") {
      return new Just(Complete.value);
    }
    ;
    return Nothing.value;
  };

  // output/Web.HTML.HTMLDocument/index.js
  var map7 = /* @__PURE__ */ map(functorEffect);
  var toParentNode = unsafeCoerce2;
  var toDocument = unsafeCoerce2;
  var readyState = function(doc) {
    return map7(function() {
      var $4 = fromMaybe(Loading.value);
      return function($5) {
        return $4(parse($5));
      };
    }())(function() {
      return _readyState(doc);
    });
  };

  // output/Web.HTML.HTMLElement/foreign.js
  function _read(nothing, just, value15) {
    var tag = Object.prototype.toString.call(value15);
    if (tag.indexOf("[object HTML") === 0 && tag.indexOf("Element]") === tag.length - 8) {
      return just(value15);
    } else {
      return nothing;
    }
  }

  // output/Web.HTML.HTMLElement/index.js
  var toNode = unsafeCoerce2;
  var fromElement = function(x) {
    return _read(Nothing.value, Just.create, x);
  };

  // output/Effect.Uncurried/foreign.js
  var runEffectFn1 = function runEffectFn12(fn) {
    return function(a2) {
      return function() {
        return fn(a2);
      };
    };
  };
  var runEffectFn4 = function runEffectFn42(fn) {
    return function(a2) {
      return function(b2) {
        return function(c) {
          return function(d) {
            return function() {
              return fn(a2, b2, c, d);
            };
          };
        };
      };
    };
  };

  // output/Data.Enum/foreign.js
  function toCharCode(c) {
    return c.charCodeAt(0);
  }
  function fromCharCode(c) {
    return String.fromCharCode(c);
  }

  // output/Control.Alternative/index.js
  var guard = function(dictAlternative) {
    var pure21 = pure(dictAlternative.Applicative0());
    var empty8 = empty(dictAlternative.Plus1());
    return function(v) {
      if (v) {
        return pure21(unit);
      }
      ;
      if (!v) {
        return empty8;
      }
      ;
      throw new Error("Failed pattern match at Control.Alternative (line 48, column 1 - line 48, column 54): " + [v.constructor.name]);
    };
  };

  // output/Data.Unfoldable/foreign.js
  var unfoldrArrayImpl = function(isNothing2) {
    return function(fromJust7) {
      return function(fst2) {
        return function(snd2) {
          return function(f) {
            return function(b2) {
              var result = [];
              var value15 = b2;
              while (true) {
                var maybe2 = f(value15);
                if (isNothing2(maybe2))
                  return result;
                var tuple = fromJust7(maybe2);
                result.push(fst2(tuple));
                value15 = snd2(tuple);
              }
            };
          };
        };
      };
    };
  };

  // output/Data.Unfoldable1/foreign.js
  var unfoldr1ArrayImpl = function(isNothing2) {
    return function(fromJust7) {
      return function(fst2) {
        return function(snd2) {
          return function(f) {
            return function(b2) {
              var result = [];
              var value15 = b2;
              while (true) {
                var tuple = f(value15);
                result.push(fst2(tuple));
                var maybe2 = snd2(tuple);
                if (isNothing2(maybe2))
                  return result;
                value15 = fromJust7(maybe2);
              }
            };
          };
        };
      };
    };
  };

  // output/Data.Unfoldable1/index.js
  var fromJust2 = /* @__PURE__ */ fromJust();
  var unfoldr1 = function(dict) {
    return dict.unfoldr1;
  };
  var unfoldable1Array = {
    unfoldr1: /* @__PURE__ */ unfoldr1ArrayImpl(isNothing)(fromJust2)(fst)(snd)
  };

  // output/Data.Unfoldable/index.js
  var fromJust3 = /* @__PURE__ */ fromJust();
  var unfoldr = function(dict) {
    return dict.unfoldr;
  };
  var unfoldableArray = {
    unfoldr: /* @__PURE__ */ unfoldrArrayImpl(isNothing)(fromJust3)(fst)(snd),
    Unfoldable10: function() {
      return unfoldable1Array;
    }
  };
  var replicate = function(dictUnfoldable) {
    var unfoldr13 = unfoldr(dictUnfoldable);
    return function(n) {
      return function(v) {
        var step4 = function(i2) {
          var $17 = i2 <= 0;
          if ($17) {
            return Nothing.value;
          }
          ;
          return new Just(new Tuple(v, i2 - 1 | 0));
        };
        return unfoldr13(step4)(n);
      };
    };
  };
  var replicateA = function(dictApplicative) {
    return function(dictUnfoldable) {
      var replicate12 = replicate(dictUnfoldable);
      return function(dictTraversable) {
        var sequence2 = sequence(dictTraversable)(dictApplicative);
        return function(n) {
          return function(m) {
            return sequence2(replicate12(n)(m));
          };
        };
      };
    };
  };

  // output/Data.Enum/index.js
  var bottom1 = /* @__PURE__ */ bottom(boundedChar);
  var top1 = /* @__PURE__ */ top(boundedChar);
  var toEnum = function(dict) {
    return dict.toEnum;
  };
  var fromEnum = function(dict) {
    return dict.fromEnum;
  };
  var toEnumWithDefaults = function(dictBoundedEnum) {
    var toEnum12 = toEnum(dictBoundedEnum);
    var fromEnum12 = fromEnum(dictBoundedEnum);
    var bottom22 = bottom(dictBoundedEnum.Bounded0());
    return function(low2) {
      return function(high2) {
        return function(x) {
          var v = toEnum12(x);
          if (v instanceof Just) {
            return v.value0;
          }
          ;
          if (v instanceof Nothing) {
            var $140 = x < fromEnum12(bottom22);
            if ($140) {
              return low2;
            }
            ;
            return high2;
          }
          ;
          throw new Error("Failed pattern match at Data.Enum (line 158, column 33 - line 160, column 62): " + [v.constructor.name]);
        };
      };
    };
  };
  var defaultSucc = function(toEnum$prime) {
    return function(fromEnum$prime) {
      return function(a2) {
        return toEnum$prime(fromEnum$prime(a2) + 1 | 0);
      };
    };
  };
  var defaultPred = function(toEnum$prime) {
    return function(fromEnum$prime) {
      return function(a2) {
        return toEnum$prime(fromEnum$prime(a2) - 1 | 0);
      };
    };
  };
  var charToEnum = function(v) {
    if (v >= toCharCode(bottom1) && v <= toCharCode(top1)) {
      return new Just(fromCharCode(v));
    }
    ;
    return Nothing.value;
  };
  var enumChar = {
    succ: /* @__PURE__ */ defaultSucc(charToEnum)(toCharCode),
    pred: /* @__PURE__ */ defaultPred(charToEnum)(toCharCode),
    Ord0: function() {
      return ordChar;
    }
  };
  var boundedEnumChar = /* @__PURE__ */ function() {
    return {
      cardinality: toCharCode(top1) - toCharCode(bottom1) | 0,
      toEnum: charToEnum,
      fromEnum: toCharCode,
      Bounded0: function() {
        return boundedChar;
      },
      Enum1: function() {
        return enumChar;
      }
    };
  }();

  // output/Web.HTML.Window/foreign.js
  function document2(window2) {
    return function() {
      return window2.document;
    };
  }
  function navigator2(window2) {
    return function() {
      return window2.navigator;
    };
  }
  function requestIdleCallback(opts) {
    return function(fn) {
      return function(window2) {
        return function() {
          return window2.requestIdleCallback(fn, opts);
        };
      };
    };
  }
  function cancelIdleCallback(id3) {
    return function(window2) {
      return function() {
        return window2.cancelIdleCallback(id3);
      };
    };
  }

  // output/Web.HTML.Window/index.js
  var toEventTarget = unsafeCoerce2;

  // output/Web.HTML.Event.EventTypes/index.js
  var input = "input";
  var domcontentloaded = "DOMContentLoaded";
  var change = "change";

  // output/Halogen.Aff.Util/index.js
  var bind2 = /* @__PURE__ */ bind(bindAff);
  var liftEffect3 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var bindFlipped4 = /* @__PURE__ */ bindFlipped(bindEffect);
  var composeKleisliFlipped2 = /* @__PURE__ */ composeKleisliFlipped(bindEffect);
  var pure3 = /* @__PURE__ */ pure(applicativeAff);
  var bindFlipped1 = /* @__PURE__ */ bindFlipped(bindMaybe);
  var pure1 = /* @__PURE__ */ pure(applicativeEffect);
  var map8 = /* @__PURE__ */ map(functorEffect);
  var discard2 = /* @__PURE__ */ discard(discardUnit);
  var throwError2 = /* @__PURE__ */ throwError(monadThrowAff);
  var selectElement = function(query3) {
    return bind2(liftEffect3(bindFlipped4(composeKleisliFlipped2(function() {
      var $16 = querySelector(query3);
      return function($17) {
        return $16(toParentNode($17));
      };
    }())(document2))(windowImpl)))(function(mel) {
      return pure3(bindFlipped1(fromElement)(mel));
    });
  };
  var runHalogenAff = /* @__PURE__ */ runAff_(/* @__PURE__ */ either(throwException)(/* @__PURE__ */ $$const(/* @__PURE__ */ pure1(unit))));
  var awaitLoad = /* @__PURE__ */ makeAff(function(callback) {
    return function __do2() {
      var rs = bindFlipped4(readyState)(bindFlipped4(document2)(windowImpl))();
      if (rs instanceof Loading) {
        var et = map8(toEventTarget)(windowImpl)();
        var listener = eventListener(function(v) {
          return callback(new Right(unit));
        })();
        addEventListener(domcontentloaded)(listener)(false)(et)();
        return effectCanceler(removeEventListener(domcontentloaded)(listener)(false)(et));
      }
      ;
      callback(new Right(unit))();
      return nonCanceler;
    };
  });
  var awaitBody = /* @__PURE__ */ discard2(bindAff)(awaitLoad)(function() {
    return bind2(selectElement("body"))(function(body2) {
      return maybe(throwError2(error("Could not find body")))(pure3)(body2);
    });
  });

  // output/Control.Monad.Fork.Class/index.js
  var monadForkAff = {
    suspend: suspendAff,
    fork: forkAff,
    join: joinFiber,
    Monad0: function() {
      return monadAff;
    },
    Functor1: function() {
      return functorFiber;
    }
  };
  var fork = function(dict) {
    return dict.fork;
  };

  // output/Data.FunctorWithIndex/foreign.js
  var mapWithIndexArray = function(f) {
    return function(xs) {
      var l = xs.length;
      var result = Array(l);
      for (var i2 = 0; i2 < l; i2++) {
        result[i2] = f(i2)(xs[i2]);
      }
      return result;
    };
  };

  // output/Data.FunctorWithIndex/index.js
  var mapWithIndex = function(dict) {
    return dict.mapWithIndex;
  };
  var functorWithIndexArray = {
    mapWithIndex: mapWithIndexArray,
    Functor0: function() {
      return functorArray;
    }
  };

  // output/Data.NonEmpty/index.js
  var NonEmpty = /* @__PURE__ */ function() {
    function NonEmpty2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    NonEmpty2.create = function(value0) {
      return function(value1) {
        return new NonEmpty2(value0, value1);
      };
    };
    return NonEmpty2;
  }();
  var singleton2 = function(dictPlus) {
    var empty8 = empty(dictPlus);
    return function(a2) {
      return new NonEmpty(a2, empty8);
    };
  };

  // output/Data.List.Types/index.js
  var Nil = /* @__PURE__ */ function() {
    function Nil3() {
    }
    ;
    Nil3.value = new Nil3();
    return Nil3;
  }();
  var Cons = /* @__PURE__ */ function() {
    function Cons3(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Cons3.create = function(value0) {
      return function(value1) {
        return new Cons3(value0, value1);
      };
    };
    return Cons3;
  }();
  var NonEmptyList = function(x) {
    return x;
  };
  var toList = function(v) {
    return new Cons(v.value0, v.value1);
  };
  var listMap = function(f) {
    var chunkedRevMap = function($copy_v) {
      return function($copy_v1) {
        var $tco_var_v = $copy_v;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v, v1) {
          if (v1 instanceof Cons && (v1.value1 instanceof Cons && v1.value1.value1 instanceof Cons)) {
            $tco_var_v = new Cons(v1, v);
            $copy_v1 = v1.value1.value1.value1;
            return;
          }
          ;
          var unrolledMap = function(v2) {
            if (v2 instanceof Cons && (v2.value1 instanceof Cons && v2.value1.value1 instanceof Nil)) {
              return new Cons(f(v2.value0), new Cons(f(v2.value1.value0), Nil.value));
            }
            ;
            if (v2 instanceof Cons && v2.value1 instanceof Nil) {
              return new Cons(f(v2.value0), Nil.value);
            }
            ;
            return Nil.value;
          };
          var reverseUnrolledMap = function($copy_v2) {
            return function($copy_v3) {
              var $tco_var_v2 = $copy_v2;
              var $tco_done1 = false;
              var $tco_result2;
              function $tco_loop2(v2, v3) {
                if (v2 instanceof Cons && (v2.value0 instanceof Cons && (v2.value0.value1 instanceof Cons && v2.value0.value1.value1 instanceof Cons))) {
                  $tco_var_v2 = v2.value1;
                  $copy_v3 = new Cons(f(v2.value0.value0), new Cons(f(v2.value0.value1.value0), new Cons(f(v2.value0.value1.value1.value0), v3)));
                  return;
                }
                ;
                $tco_done1 = true;
                return v3;
              }
              ;
              while (!$tco_done1) {
                $tco_result2 = $tco_loop2($tco_var_v2, $copy_v3);
              }
              ;
              return $tco_result2;
            };
          };
          $tco_done = true;
          return reverseUnrolledMap(v)(unrolledMap(v1));
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_v, $copy_v1);
        }
        ;
        return $tco_result;
      };
    };
    return chunkedRevMap(Nil.value);
  };
  var functorList = {
    map: listMap
  };
  var foldableList = {
    foldr: function(f) {
      return function(b2) {
        var rev3 = function() {
          var go2 = function($copy_v) {
            return function($copy_v1) {
              var $tco_var_v = $copy_v;
              var $tco_done = false;
              var $tco_result;
              function $tco_loop(v, v1) {
                if (v1 instanceof Nil) {
                  $tco_done = true;
                  return v;
                }
                ;
                if (v1 instanceof Cons) {
                  $tco_var_v = new Cons(v1.value0, v);
                  $copy_v1 = v1.value1;
                  return;
                }
                ;
                throw new Error("Failed pattern match at Data.List.Types (line 107, column 7 - line 107, column 23): " + [v.constructor.name, v1.constructor.name]);
              }
              ;
              while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_v, $copy_v1);
              }
              ;
              return $tco_result;
            };
          };
          return go2(Nil.value);
        }();
        var $284 = foldl(foldableList)(flip(f))(b2);
        return function($285) {
          return $284(rev3($285));
        };
      };
    },
    foldl: function(f) {
      var go2 = function($copy_b) {
        return function($copy_v) {
          var $tco_var_b = $copy_b;
          var $tco_done1 = false;
          var $tco_result;
          function $tco_loop(b2, v) {
            if (v instanceof Nil) {
              $tco_done1 = true;
              return b2;
            }
            ;
            if (v instanceof Cons) {
              $tco_var_b = f(b2)(v.value0);
              $copy_v = v.value1;
              return;
            }
            ;
            throw new Error("Failed pattern match at Data.List.Types (line 111, column 12 - line 113, column 30): " + [v.constructor.name]);
          }
          ;
          while (!$tco_done1) {
            $tco_result = $tco_loop($tco_var_b, $copy_v);
          }
          ;
          return $tco_result;
        };
      };
      return go2;
    },
    foldMap: function(dictMonoid) {
      var append22 = append(dictMonoid.Semigroup0());
      var mempty6 = mempty(dictMonoid);
      return function(f) {
        return foldl(foldableList)(function(acc) {
          var $286 = append22(acc);
          return function($287) {
            return $286(f($287));
          };
        })(mempty6);
      };
    }
  };
  var foldr2 = /* @__PURE__ */ foldr(foldableList);
  var semigroupList = {
    append: function(xs) {
      return function(ys) {
        return foldr2(Cons.create)(ys)(xs);
      };
    }
  };
  var append1 = /* @__PURE__ */ append(semigroupList);
  var semigroupNonEmptyList = {
    append: function(v) {
      return function(as$prime) {
        return new NonEmpty(v.value0, append1(v.value1)(toList(as$prime)));
      };
    }
  };
  var altList = {
    alt: append1,
    Functor0: function() {
      return functorList;
    }
  };
  var plusList = /* @__PURE__ */ function() {
    return {
      empty: Nil.value,
      Alt0: function() {
        return altList;
      }
    };
  }();

  // output/Data.List/index.js
  var uncons = function(v) {
    if (v instanceof Nil) {
      return Nothing.value;
    }
    ;
    if (v instanceof Cons) {
      return new Just({
        head: v.value0,
        tail: v.value1
      });
    }
    ;
    throw new Error("Failed pattern match at Data.List (line 259, column 1 - line 259, column 66): " + [v.constructor.name]);
  };
  var reverse = /* @__PURE__ */ function() {
    var go2 = function($copy_v) {
      return function($copy_v1) {
        var $tco_var_v = $copy_v;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v, v1) {
          if (v1 instanceof Nil) {
            $tco_done = true;
            return v;
          }
          ;
          if (v1 instanceof Cons) {
            $tco_var_v = new Cons(v1.value0, v);
            $copy_v1 = v1.value1;
            return;
          }
          ;
          throw new Error("Failed pattern match at Data.List (line 368, column 3 - line 368, column 19): " + [v.constructor.name, v1.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_v, $copy_v1);
        }
        ;
        return $tco_result;
      };
    };
    return go2(Nil.value);
  }();
  var $$null = function(v) {
    if (v instanceof Nil) {
      return true;
    }
    ;
    return false;
  };
  var head = function(v) {
    if (v instanceof Nil) {
      return Nothing.value;
    }
    ;
    if (v instanceof Cons) {
      return new Just(v.value0);
    }
    ;
    throw new Error("Failed pattern match at Data.List (line 230, column 1 - line 230, column 22): " + [v.constructor.name]);
  };
  var fromFoldable = function(dictFoldable) {
    return foldr(dictFoldable)(Cons.create)(Nil.value);
  };

  // output/Data.Map.Internal/index.js
  var Leaf = /* @__PURE__ */ function() {
    function Leaf2() {
    }
    ;
    Leaf2.value = new Leaf2();
    return Leaf2;
  }();
  var Two = /* @__PURE__ */ function() {
    function Two2(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    Two2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new Two2(value0, value1, value22, value32);
          };
        };
      };
    };
    return Two2;
  }();
  var Three = /* @__PURE__ */ function() {
    function Three2(value0, value1, value22, value32, value42, value52, value62) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
      this.value4 = value42;
      this.value5 = value52;
      this.value6 = value62;
    }
    ;
    Three2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return function(value42) {
              return function(value52) {
                return function(value62) {
                  return new Three2(value0, value1, value22, value32, value42, value52, value62);
                };
              };
            };
          };
        };
      };
    };
    return Three2;
  }();
  var TwoLeft = /* @__PURE__ */ function() {
    function TwoLeft2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    TwoLeft2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new TwoLeft2(value0, value1, value22);
        };
      };
    };
    return TwoLeft2;
  }();
  var TwoRight = /* @__PURE__ */ function() {
    function TwoRight2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    TwoRight2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new TwoRight2(value0, value1, value22);
        };
      };
    };
    return TwoRight2;
  }();
  var ThreeLeft = /* @__PURE__ */ function() {
    function ThreeLeft2(value0, value1, value22, value32, value42, value52) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
      this.value4 = value42;
      this.value5 = value52;
    }
    ;
    ThreeLeft2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return function(value42) {
              return function(value52) {
                return new ThreeLeft2(value0, value1, value22, value32, value42, value52);
              };
            };
          };
        };
      };
    };
    return ThreeLeft2;
  }();
  var ThreeMiddle = /* @__PURE__ */ function() {
    function ThreeMiddle2(value0, value1, value22, value32, value42, value52) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
      this.value4 = value42;
      this.value5 = value52;
    }
    ;
    ThreeMiddle2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return function(value42) {
              return function(value52) {
                return new ThreeMiddle2(value0, value1, value22, value32, value42, value52);
              };
            };
          };
        };
      };
    };
    return ThreeMiddle2;
  }();
  var ThreeRight = /* @__PURE__ */ function() {
    function ThreeRight2(value0, value1, value22, value32, value42, value52) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
      this.value4 = value42;
      this.value5 = value52;
    }
    ;
    ThreeRight2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return function(value42) {
              return function(value52) {
                return new ThreeRight2(value0, value1, value22, value32, value42, value52);
              };
            };
          };
        };
      };
    };
    return ThreeRight2;
  }();
  var KickUp = /* @__PURE__ */ function() {
    function KickUp2(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    KickUp2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new KickUp2(value0, value1, value22, value32);
          };
        };
      };
    };
    return KickUp2;
  }();
  var lookup = function(dictOrd) {
    var compare3 = compare(dictOrd);
    return function(k) {
      var go2 = function($copy_v) {
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v) {
          if (v instanceof Leaf) {
            $tco_done = true;
            return Nothing.value;
          }
          ;
          if (v instanceof Two) {
            var v2 = compare3(k)(v.value1);
            if (v2 instanceof EQ) {
              $tco_done = true;
              return new Just(v.value2);
            }
            ;
            if (v2 instanceof LT) {
              $copy_v = v.value0;
              return;
            }
            ;
            $copy_v = v.value3;
            return;
          }
          ;
          if (v instanceof Three) {
            var v3 = compare3(k)(v.value1);
            if (v3 instanceof EQ) {
              $tco_done = true;
              return new Just(v.value2);
            }
            ;
            var v4 = compare3(k)(v.value4);
            if (v4 instanceof EQ) {
              $tco_done = true;
              return new Just(v.value5);
            }
            ;
            if (v3 instanceof LT) {
              $copy_v = v.value0;
              return;
            }
            ;
            if (v4 instanceof GT) {
              $copy_v = v.value6;
              return;
            }
            ;
            $copy_v = v.value3;
            return;
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 241, column 5 - line 241, column 22): " + [v.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($copy_v);
        }
        ;
        return $tco_result;
      };
      return go2;
    };
  };
  var fromZipper = function($copy_dictOrd) {
    return function($copy_v) {
      return function($copy_v1) {
        var $tco_var_dictOrd = $copy_dictOrd;
        var $tco_var_v = $copy_v;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(dictOrd, v, v1) {
          if (v instanceof Nil) {
            $tco_done = true;
            return v1;
          }
          ;
          if (v instanceof Cons) {
            if (v.value0 instanceof TwoLeft) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_v1 = new Two(v1, v.value0.value0, v.value0.value1, v.value0.value2);
              return;
            }
            ;
            if (v.value0 instanceof TwoRight) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_v1 = new Two(v.value0.value0, v.value0.value1, v.value0.value2, v1);
              return;
            }
            ;
            if (v.value0 instanceof ThreeLeft) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_v1 = new Three(v1, v.value0.value0, v.value0.value1, v.value0.value2, v.value0.value3, v.value0.value4, v.value0.value5);
              return;
            }
            ;
            if (v.value0 instanceof ThreeMiddle) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_v1 = new Three(v.value0.value0, v.value0.value1, v.value0.value2, v1, v.value0.value3, v.value0.value4, v.value0.value5);
              return;
            }
            ;
            if (v.value0 instanceof ThreeRight) {
              $tco_var_dictOrd = dictOrd;
              $tco_var_v = v.value1;
              $copy_v1 = new Three(v.value0.value0, v.value0.value1, v.value0.value2, v.value0.value3, v.value0.value4, v.value0.value5, v1);
              return;
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 462, column 3 - line 467, column 88): " + [v.value0.constructor.name]);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 459, column 1 - line 459, column 80): " + [v.constructor.name, v1.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_dictOrd, $tco_var_v, $copy_v1);
        }
        ;
        return $tco_result;
      };
    };
  };
  var insert = function(dictOrd) {
    var fromZipper1 = fromZipper(dictOrd);
    var compare3 = compare(dictOrd);
    return function(k) {
      return function(v) {
        var up = function($copy_v1) {
          return function($copy_v2) {
            var $tco_var_v1 = $copy_v1;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(v1, v2) {
              if (v1 instanceof Nil) {
                $tco_done = true;
                return new Two(v2.value0, v2.value1, v2.value2, v2.value3);
              }
              ;
              if (v1 instanceof Cons) {
                if (v1.value0 instanceof TwoLeft) {
                  $tco_done = true;
                  return fromZipper1(v1.value1)(new Three(v2.value0, v2.value1, v2.value2, v2.value3, v1.value0.value0, v1.value0.value1, v1.value0.value2));
                }
                ;
                if (v1.value0 instanceof TwoRight) {
                  $tco_done = true;
                  return fromZipper1(v1.value1)(new Three(v1.value0.value0, v1.value0.value1, v1.value0.value2, v2.value0, v2.value1, v2.value2, v2.value3));
                }
                ;
                if (v1.value0 instanceof ThreeLeft) {
                  $tco_var_v1 = v1.value1;
                  $copy_v2 = new KickUp(new Two(v2.value0, v2.value1, v2.value2, v2.value3), v1.value0.value0, v1.value0.value1, new Two(v1.value0.value2, v1.value0.value3, v1.value0.value4, v1.value0.value5));
                  return;
                }
                ;
                if (v1.value0 instanceof ThreeMiddle) {
                  $tco_var_v1 = v1.value1;
                  $copy_v2 = new KickUp(new Two(v1.value0.value0, v1.value0.value1, v1.value0.value2, v2.value0), v2.value1, v2.value2, new Two(v2.value3, v1.value0.value3, v1.value0.value4, v1.value0.value5));
                  return;
                }
                ;
                if (v1.value0 instanceof ThreeRight) {
                  $tco_var_v1 = v1.value1;
                  $copy_v2 = new KickUp(new Two(v1.value0.value0, v1.value0.value1, v1.value0.value2, v1.value0.value3), v1.value0.value4, v1.value0.value5, new Two(v2.value0, v2.value1, v2.value2, v2.value3));
                  return;
                }
                ;
                throw new Error("Failed pattern match at Data.Map.Internal (line 498, column 5 - line 503, column 108): " + [v1.value0.constructor.name, v2.constructor.name]);
              }
              ;
              throw new Error("Failed pattern match at Data.Map.Internal (line 495, column 3 - line 495, column 56): " + [v1.constructor.name, v2.constructor.name]);
            }
            ;
            while (!$tco_done) {
              $tco_result = $tco_loop($tco_var_v1, $copy_v2);
            }
            ;
            return $tco_result;
          };
        };
        var down = function($copy_v1) {
          return function($copy_v2) {
            var $tco_var_v1 = $copy_v1;
            var $tco_done1 = false;
            var $tco_result;
            function $tco_loop(v1, v2) {
              if (v2 instanceof Leaf) {
                $tco_done1 = true;
                return up(v1)(new KickUp(Leaf.value, k, v, Leaf.value));
              }
              ;
              if (v2 instanceof Two) {
                var v3 = compare3(k)(v2.value1);
                if (v3 instanceof EQ) {
                  $tco_done1 = true;
                  return fromZipper1(v1)(new Two(v2.value0, k, v, v2.value3));
                }
                ;
                if (v3 instanceof LT) {
                  $tco_var_v1 = new Cons(new TwoLeft(v2.value1, v2.value2, v2.value3), v1);
                  $copy_v2 = v2.value0;
                  return;
                }
                ;
                $tco_var_v1 = new Cons(new TwoRight(v2.value0, v2.value1, v2.value2), v1);
                $copy_v2 = v2.value3;
                return;
              }
              ;
              if (v2 instanceof Three) {
                var v3 = compare3(k)(v2.value1);
                if (v3 instanceof EQ) {
                  $tco_done1 = true;
                  return fromZipper1(v1)(new Three(v2.value0, k, v, v2.value3, v2.value4, v2.value5, v2.value6));
                }
                ;
                var v4 = compare3(k)(v2.value4);
                if (v4 instanceof EQ) {
                  $tco_done1 = true;
                  return fromZipper1(v1)(new Three(v2.value0, v2.value1, v2.value2, v2.value3, k, v, v2.value6));
                }
                ;
                if (v3 instanceof LT) {
                  $tco_var_v1 = new Cons(new ThreeLeft(v2.value1, v2.value2, v2.value3, v2.value4, v2.value5, v2.value6), v1);
                  $copy_v2 = v2.value0;
                  return;
                }
                ;
                if (v3 instanceof GT && v4 instanceof LT) {
                  $tco_var_v1 = new Cons(new ThreeMiddle(v2.value0, v2.value1, v2.value2, v2.value4, v2.value5, v2.value6), v1);
                  $copy_v2 = v2.value3;
                  return;
                }
                ;
                $tco_var_v1 = new Cons(new ThreeRight(v2.value0, v2.value1, v2.value2, v2.value3, v2.value4, v2.value5), v1);
                $copy_v2 = v2.value6;
                return;
              }
              ;
              throw new Error("Failed pattern match at Data.Map.Internal (line 478, column 3 - line 478, column 55): " + [v1.constructor.name, v2.constructor.name]);
            }
            ;
            while (!$tco_done1) {
              $tco_result = $tco_loop($tco_var_v1, $copy_v2);
            }
            ;
            return $tco_result;
          };
        };
        return down(Nil.value);
      };
    };
  };
  var pop = function(dictOrd) {
    var fromZipper1 = fromZipper(dictOrd);
    var compare3 = compare(dictOrd);
    return function(k) {
      var up = function($copy_ctxs) {
        return function($copy_tree) {
          var $tco_var_ctxs = $copy_ctxs;
          var $tco_done = false;
          var $tco_result;
          function $tco_loop(ctxs, tree) {
            if (ctxs instanceof Nil) {
              $tco_done = true;
              return tree;
            }
            ;
            if (ctxs instanceof Cons) {
              if (ctxs.value0 instanceof TwoLeft && (ctxs.value0.value2 instanceof Leaf && tree instanceof Leaf)) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(Leaf.value, ctxs.value0.value0, ctxs.value0.value1, Leaf.value));
              }
              ;
              if (ctxs.value0 instanceof TwoRight && (ctxs.value0.value0 instanceof Leaf && tree instanceof Leaf)) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(Leaf.value, ctxs.value0.value1, ctxs.value0.value2, Leaf.value));
              }
              ;
              if (ctxs.value0 instanceof TwoLeft && ctxs.value0.value2 instanceof Two) {
                $tco_var_ctxs = ctxs.value1;
                $copy_tree = new Three(tree, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0, ctxs.value0.value2.value1, ctxs.value0.value2.value2, ctxs.value0.value2.value3);
                return;
              }
              ;
              if (ctxs.value0 instanceof TwoRight && ctxs.value0.value0 instanceof Two) {
                $tco_var_ctxs = ctxs.value1;
                $copy_tree = new Three(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3, ctxs.value0.value1, ctxs.value0.value2, tree);
                return;
              }
              ;
              if (ctxs.value0 instanceof TwoLeft && ctxs.value0.value2 instanceof Three) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(new Two(tree, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0), ctxs.value0.value2.value1, ctxs.value0.value2.value2, new Two(ctxs.value0.value2.value3, ctxs.value0.value2.value4, ctxs.value0.value2.value5, ctxs.value0.value2.value6)));
              }
              ;
              if (ctxs.value0 instanceof TwoRight && ctxs.value0.value0 instanceof Three) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(new Two(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3), ctxs.value0.value0.value4, ctxs.value0.value0.value5, new Two(ctxs.value0.value0.value6, ctxs.value0.value1, ctxs.value0.value2, tree)));
              }
              ;
              if (ctxs.value0 instanceof ThreeLeft && (ctxs.value0.value2 instanceof Leaf && (ctxs.value0.value5 instanceof Leaf && tree instanceof Leaf))) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Three(Leaf.value, ctxs.value0.value0, ctxs.value0.value1, Leaf.value, ctxs.value0.value3, ctxs.value0.value4, Leaf.value));
              }
              ;
              if (ctxs.value0 instanceof ThreeMiddle && (ctxs.value0.value0 instanceof Leaf && (ctxs.value0.value5 instanceof Leaf && tree instanceof Leaf))) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Three(Leaf.value, ctxs.value0.value1, ctxs.value0.value2, Leaf.value, ctxs.value0.value3, ctxs.value0.value4, Leaf.value));
              }
              ;
              if (ctxs.value0 instanceof ThreeRight && (ctxs.value0.value0 instanceof Leaf && (ctxs.value0.value3 instanceof Leaf && tree instanceof Leaf))) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Three(Leaf.value, ctxs.value0.value1, ctxs.value0.value2, Leaf.value, ctxs.value0.value4, ctxs.value0.value5, Leaf.value));
              }
              ;
              if (ctxs.value0 instanceof ThreeLeft && ctxs.value0.value2 instanceof Two) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(new Three(tree, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0, ctxs.value0.value2.value1, ctxs.value0.value2.value2, ctxs.value0.value2.value3), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
              }
              ;
              if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value0 instanceof Two) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(new Three(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3, ctxs.value0.value1, ctxs.value0.value2, tree), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
              }
              ;
              if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value5 instanceof Two) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Three(tree, ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5.value0, ctxs.value0.value5.value1, ctxs.value0.value5.value2, ctxs.value0.value5.value3)));
              }
              ;
              if (ctxs.value0 instanceof ThreeRight && ctxs.value0.value3 instanceof Two) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Two(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Three(ctxs.value0.value3.value0, ctxs.value0.value3.value1, ctxs.value0.value3.value2, ctxs.value0.value3.value3, ctxs.value0.value4, ctxs.value0.value5, tree)));
              }
              ;
              if (ctxs.value0 instanceof ThreeLeft && ctxs.value0.value2 instanceof Three) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Three(new Two(tree, ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2.value0), ctxs.value0.value2.value1, ctxs.value0.value2.value2, new Two(ctxs.value0.value2.value3, ctxs.value0.value2.value4, ctxs.value0.value2.value5, ctxs.value0.value2.value6), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
              }
              ;
              if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value0 instanceof Three) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Three(new Two(ctxs.value0.value0.value0, ctxs.value0.value0.value1, ctxs.value0.value0.value2, ctxs.value0.value0.value3), ctxs.value0.value0.value4, ctxs.value0.value0.value5, new Two(ctxs.value0.value0.value6, ctxs.value0.value1, ctxs.value0.value2, tree), ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5));
              }
              ;
              if (ctxs.value0 instanceof ThreeMiddle && ctxs.value0.value5 instanceof Three) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Three(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Two(tree, ctxs.value0.value3, ctxs.value0.value4, ctxs.value0.value5.value0), ctxs.value0.value5.value1, ctxs.value0.value5.value2, new Two(ctxs.value0.value5.value3, ctxs.value0.value5.value4, ctxs.value0.value5.value5, ctxs.value0.value5.value6)));
              }
              ;
              if (ctxs.value0 instanceof ThreeRight && ctxs.value0.value3 instanceof Three) {
                $tco_done = true;
                return fromZipper1(ctxs.value1)(new Three(ctxs.value0.value0, ctxs.value0.value1, ctxs.value0.value2, new Two(ctxs.value0.value3.value0, ctxs.value0.value3.value1, ctxs.value0.value3.value2, ctxs.value0.value3.value3), ctxs.value0.value3.value4, ctxs.value0.value3.value5, new Two(ctxs.value0.value3.value6, ctxs.value0.value4, ctxs.value0.value5, tree)));
              }
              ;
              $tco_done = true;
              return unsafeCrashWith("The impossible happened in partial function `up`.");
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 552, column 5 - line 573, column 86): " + [ctxs.constructor.name]);
          }
          ;
          while (!$tco_done) {
            $tco_result = $tco_loop($tco_var_ctxs, $copy_tree);
          }
          ;
          return $tco_result;
        };
      };
      var removeMaxNode = function($copy_ctx) {
        return function($copy_m) {
          var $tco_var_ctx = $copy_ctx;
          var $tco_done1 = false;
          var $tco_result;
          function $tco_loop(ctx, m) {
            if (m instanceof Two && (m.value0 instanceof Leaf && m.value3 instanceof Leaf)) {
              $tco_done1 = true;
              return up(ctx)(Leaf.value);
            }
            ;
            if (m instanceof Two) {
              $tco_var_ctx = new Cons(new TwoRight(m.value0, m.value1, m.value2), ctx);
              $copy_m = m.value3;
              return;
            }
            ;
            if (m instanceof Three && (m.value0 instanceof Leaf && (m.value3 instanceof Leaf && m.value6 instanceof Leaf))) {
              $tco_done1 = true;
              return up(new Cons(new TwoRight(Leaf.value, m.value1, m.value2), ctx))(Leaf.value);
            }
            ;
            if (m instanceof Three) {
              $tco_var_ctx = new Cons(new ThreeRight(m.value0, m.value1, m.value2, m.value3, m.value4, m.value5), ctx);
              $copy_m = m.value6;
              return;
            }
            ;
            $tco_done1 = true;
            return unsafeCrashWith("The impossible happened in partial function `removeMaxNode`.");
          }
          ;
          while (!$tco_done1) {
            $tco_result = $tco_loop($tco_var_ctx, $copy_m);
          }
          ;
          return $tco_result;
        };
      };
      var maxNode = function($copy_m) {
        var $tco_done2 = false;
        var $tco_result;
        function $tco_loop(m) {
          if (m instanceof Two && m.value3 instanceof Leaf) {
            $tco_done2 = true;
            return {
              key: m.value1,
              value: m.value2
            };
          }
          ;
          if (m instanceof Two) {
            $copy_m = m.value3;
            return;
          }
          ;
          if (m instanceof Three && m.value6 instanceof Leaf) {
            $tco_done2 = true;
            return {
              key: m.value4,
              value: m.value5
            };
          }
          ;
          if (m instanceof Three) {
            $copy_m = m.value6;
            return;
          }
          ;
          $tco_done2 = true;
          return unsafeCrashWith("The impossible happened in partial function `maxNode`.");
        }
        ;
        while (!$tco_done2) {
          $tco_result = $tco_loop($copy_m);
        }
        ;
        return $tco_result;
      };
      var down = function($copy_ctx) {
        return function($copy_m) {
          var $tco_var_ctx = $copy_ctx;
          var $tco_done3 = false;
          var $tco_result;
          function $tco_loop(ctx, m) {
            if (m instanceof Leaf) {
              $tco_done3 = true;
              return Nothing.value;
            }
            ;
            if (m instanceof Two) {
              var v = compare3(k)(m.value1);
              if (m.value3 instanceof Leaf && v instanceof EQ) {
                $tco_done3 = true;
                return new Just(new Tuple(m.value2, up(ctx)(Leaf.value)));
              }
              ;
              if (v instanceof EQ) {
                var max6 = maxNode(m.value0);
                $tco_done3 = true;
                return new Just(new Tuple(m.value2, removeMaxNode(new Cons(new TwoLeft(max6.key, max6.value, m.value3), ctx))(m.value0)));
              }
              ;
              if (v instanceof LT) {
                $tco_var_ctx = new Cons(new TwoLeft(m.value1, m.value2, m.value3), ctx);
                $copy_m = m.value0;
                return;
              }
              ;
              $tco_var_ctx = new Cons(new TwoRight(m.value0, m.value1, m.value2), ctx);
              $copy_m = m.value3;
              return;
            }
            ;
            if (m instanceof Three) {
              var leaves = function() {
                if (m.value0 instanceof Leaf && (m.value3 instanceof Leaf && m.value6 instanceof Leaf)) {
                  return true;
                }
                ;
                return false;
              }();
              var v = compare3(k)(m.value4);
              var v3 = compare3(k)(m.value1);
              if (leaves && v3 instanceof EQ) {
                $tco_done3 = true;
                return new Just(new Tuple(m.value2, fromZipper1(ctx)(new Two(Leaf.value, m.value4, m.value5, Leaf.value))));
              }
              ;
              if (leaves && v instanceof EQ) {
                $tco_done3 = true;
                return new Just(new Tuple(m.value5, fromZipper1(ctx)(new Two(Leaf.value, m.value1, m.value2, Leaf.value))));
              }
              ;
              if (v3 instanceof EQ) {
                var max6 = maxNode(m.value0);
                $tco_done3 = true;
                return new Just(new Tuple(m.value2, removeMaxNode(new Cons(new ThreeLeft(max6.key, max6.value, m.value3, m.value4, m.value5, m.value6), ctx))(m.value0)));
              }
              ;
              if (v instanceof EQ) {
                var max6 = maxNode(m.value3);
                $tco_done3 = true;
                return new Just(new Tuple(m.value5, removeMaxNode(new Cons(new ThreeMiddle(m.value0, m.value1, m.value2, max6.key, max6.value, m.value6), ctx))(m.value3)));
              }
              ;
              if (v3 instanceof LT) {
                $tco_var_ctx = new Cons(new ThreeLeft(m.value1, m.value2, m.value3, m.value4, m.value5, m.value6), ctx);
                $copy_m = m.value0;
                return;
              }
              ;
              if (v3 instanceof GT && v instanceof LT) {
                $tco_var_ctx = new Cons(new ThreeMiddle(m.value0, m.value1, m.value2, m.value4, m.value5, m.value6), ctx);
                $copy_m = m.value3;
                return;
              }
              ;
              $tco_var_ctx = new Cons(new ThreeRight(m.value0, m.value1, m.value2, m.value3, m.value4, m.value5), ctx);
              $copy_m = m.value6;
              return;
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 525, column 16 - line 548, column 80): " + [m.constructor.name]);
          }
          ;
          while (!$tco_done3) {
            $tco_result = $tco_loop($tco_var_ctx, $copy_m);
          }
          ;
          return $tco_result;
        };
      };
      return down(Nil.value);
    };
  };
  var foldableMap = {
    foldr: function(f) {
      return function(z) {
        return function(m) {
          if (m instanceof Leaf) {
            return z;
          }
          ;
          if (m instanceof Two) {
            return foldr(foldableMap)(f)(f(m.value2)(foldr(foldableMap)(f)(z)(m.value3)))(m.value0);
          }
          ;
          if (m instanceof Three) {
            return foldr(foldableMap)(f)(f(m.value2)(foldr(foldableMap)(f)(f(m.value5)(foldr(foldableMap)(f)(z)(m.value6)))(m.value3)))(m.value0);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 133, column 17 - line 136, column 85): " + [m.constructor.name]);
        };
      };
    },
    foldl: function(f) {
      return function(z) {
        return function(m) {
          if (m instanceof Leaf) {
            return z;
          }
          ;
          if (m instanceof Two) {
            return foldl(foldableMap)(f)(f(foldl(foldableMap)(f)(z)(m.value0))(m.value2))(m.value3);
          }
          ;
          if (m instanceof Three) {
            return foldl(foldableMap)(f)(f(foldl(foldableMap)(f)(f(foldl(foldableMap)(f)(z)(m.value0))(m.value2))(m.value3))(m.value5))(m.value6);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 137, column 17 - line 140, column 85): " + [m.constructor.name]);
        };
      };
    },
    foldMap: function(dictMonoid) {
      var mempty6 = mempty(dictMonoid);
      var append22 = append(dictMonoid.Semigroup0());
      return function(f) {
        return function(m) {
          if (m instanceof Leaf) {
            return mempty6;
          }
          ;
          if (m instanceof Two) {
            return append22(foldMap(foldableMap)(dictMonoid)(f)(m.value0))(append22(f(m.value2))(foldMap(foldableMap)(dictMonoid)(f)(m.value3)));
          }
          ;
          if (m instanceof Three) {
            return append22(foldMap(foldableMap)(dictMonoid)(f)(m.value0))(append22(f(m.value2))(append22(foldMap(foldableMap)(dictMonoid)(f)(m.value3))(append22(f(m.value5))(foldMap(foldableMap)(dictMonoid)(f)(m.value6)))));
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 141, column 17 - line 144, column 93): " + [m.constructor.name]);
        };
      };
    }
  };
  var empty2 = /* @__PURE__ */ function() {
    return Leaf.value;
  }();
  var $$delete = function(dictOrd) {
    var pop12 = pop(dictOrd);
    return function(k) {
      return function(m) {
        return maybe(m)(snd)(pop12(k)(m));
      };
    };
  };
  var alter = function(dictOrd) {
    var lookup13 = lookup(dictOrd);
    var delete1 = $$delete(dictOrd);
    var insert13 = insert(dictOrd);
    return function(f) {
      return function(k) {
        return function(m) {
          var v = f(lookup13(k)(m));
          if (v instanceof Nothing) {
            return delete1(k)(m);
          }
          ;
          if (v instanceof Just) {
            return insert13(k)(v.value0)(m);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 596, column 15 - line 598, column 25): " + [v.constructor.name]);
        };
      };
    };
  };

  // output/Effect.Console/foreign.js
  var log = function(s) {
    return function() {
      console.log(s);
    };
  };
  var warn = function(s) {
    return function() {
      console.warn(s);
    };
  };

  // output/Effect.Console/index.js
  var logShow = function(dictShow) {
    var show11 = show(dictShow);
    return function(a2) {
      return log(show11(a2));
    };
  };

  // output/Data.Exists/index.js
  var runExists = unsafeCoerce2;
  var mkExists = unsafeCoerce2;

  // output/Data.Coyoneda/index.js
  var CoyonedaF = /* @__PURE__ */ function() {
    function CoyonedaF2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    CoyonedaF2.create = function(value0) {
      return function(value1) {
        return new CoyonedaF2(value0, value1);
      };
    };
    return CoyonedaF2;
  }();
  var unCoyoneda = function(f) {
    return function(v) {
      return runExists(function(v1) {
        return f(v1.value0)(v1.value1);
      })(v);
    };
  };
  var coyoneda = function(k) {
    return function(fi) {
      return mkExists(new CoyonedaF(k, fi));
    };
  };
  var functorCoyoneda = {
    map: function(f) {
      return function(v) {
        return runExists(function(v1) {
          return coyoneda(function($180) {
            return f(v1.value0($180));
          })(v1.value1);
        })(v);
      };
    }
  };
  var liftCoyoneda = /* @__PURE__ */ coyoneda(/* @__PURE__ */ identity(categoryFn));

  // output/Halogen.Data.OrdBox/index.js
  var OrdBox = /* @__PURE__ */ function() {
    function OrdBox2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    OrdBox2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new OrdBox2(value0, value1, value22);
        };
      };
    };
    return OrdBox2;
  }();
  var mkOrdBox = function(dictOrd) {
    return OrdBox.create(eq(dictOrd.Eq0()))(compare(dictOrd));
  };
  var eqOrdBox = {
    eq: function(v) {
      return function(v1) {
        return v.value0(v.value2)(v1.value2);
      };
    }
  };
  var ordOrdBox = {
    compare: function(v) {
      return function(v1) {
        return v.value1(v.value2)(v1.value2);
      };
    },
    Eq0: function() {
      return eqOrdBox;
    }
  };

  // output/Halogen.Data.Slot/index.js
  var ordTuple2 = /* @__PURE__ */ ordTuple(ordString)(ordOrdBox);
  var pop1 = /* @__PURE__ */ pop(ordTuple2);
  var lookup1 = /* @__PURE__ */ lookup(ordTuple2);
  var insert1 = /* @__PURE__ */ insert(ordTuple2);
  var pop2 = function() {
    return function(dictIsSymbol) {
      var reflectSymbol2 = reflectSymbol(dictIsSymbol);
      return function(dictOrd) {
        var mkOrdBox2 = mkOrdBox(dictOrd);
        return function(sym) {
          return function(key) {
            return function(v) {
              return pop1(new Tuple(reflectSymbol2(sym), mkOrdBox2(key)))(v);
            };
          };
        };
      };
    };
  };
  var lookup2 = function() {
    return function(dictIsSymbol) {
      var reflectSymbol2 = reflectSymbol(dictIsSymbol);
      return function(dictOrd) {
        var mkOrdBox2 = mkOrdBox(dictOrd);
        return function(sym) {
          return function(key) {
            return function(v) {
              return lookup1(new Tuple(reflectSymbol2(sym), mkOrdBox2(key)))(v);
            };
          };
        };
      };
    };
  };
  var insert2 = function() {
    return function(dictIsSymbol) {
      var reflectSymbol2 = reflectSymbol(dictIsSymbol);
      return function(dictOrd) {
        var mkOrdBox2 = mkOrdBox(dictOrd);
        return function(sym) {
          return function(key) {
            return function(val) {
              return function(v) {
                return insert1(new Tuple(reflectSymbol2(sym), mkOrdBox2(key)))(val)(v);
              };
            };
          };
        };
      };
    };
  };
  var foreachSlot = function(dictApplicative) {
    var traverse_8 = traverse_(dictApplicative)(foldableMap);
    return function(v) {
      return function(k) {
        return traverse_8(function($54) {
          return k($54);
        })(v);
      };
    };
  };
  var empty3 = empty2;

  // output/DOM.HTML.Indexed.ButtonType/index.js
  var ButtonButton = /* @__PURE__ */ function() {
    function ButtonButton2() {
    }
    ;
    ButtonButton2.value = new ButtonButton2();
    return ButtonButton2;
  }();
  var ButtonSubmit = /* @__PURE__ */ function() {
    function ButtonSubmit2() {
    }
    ;
    ButtonSubmit2.value = new ButtonSubmit2();
    return ButtonSubmit2;
  }();
  var ButtonReset = /* @__PURE__ */ function() {
    function ButtonReset2() {
    }
    ;
    ButtonReset2.value = new ButtonReset2();
    return ButtonReset2;
  }();
  var renderButtonType = function(v) {
    if (v instanceof ButtonButton) {
      return "button";
    }
    ;
    if (v instanceof ButtonSubmit) {
      return "submit";
    }
    ;
    if (v instanceof ButtonReset) {
      return "reset";
    }
    ;
    throw new Error("Failed pattern match at DOM.HTML.Indexed.ButtonType (line 14, column 20 - line 17, column 25): " + [v.constructor.name]);
  };

  // output/Data.String.Common/foreign.js
  var toUpper = function(s) {
    return s.toUpperCase();
  };
  var joinWith = function(s) {
    return function(xs) {
      return xs.join(s);
    };
  };

  // output/Data.String.Common/index.js
  var $$null2 = function(s) {
    return s === "";
  };

  // output/DOM.HTML.Indexed.InputType/index.js
  var InputButton = /* @__PURE__ */ function() {
    function InputButton2() {
    }
    ;
    InputButton2.value = new InputButton2();
    return InputButton2;
  }();
  var InputCheckbox = /* @__PURE__ */ function() {
    function InputCheckbox2() {
    }
    ;
    InputCheckbox2.value = new InputCheckbox2();
    return InputCheckbox2;
  }();
  var InputColor = /* @__PURE__ */ function() {
    function InputColor2() {
    }
    ;
    InputColor2.value = new InputColor2();
    return InputColor2;
  }();
  var InputDate = /* @__PURE__ */ function() {
    function InputDate2() {
    }
    ;
    InputDate2.value = new InputDate2();
    return InputDate2;
  }();
  var InputDatetimeLocal = /* @__PURE__ */ function() {
    function InputDatetimeLocal2() {
    }
    ;
    InputDatetimeLocal2.value = new InputDatetimeLocal2();
    return InputDatetimeLocal2;
  }();
  var InputEmail = /* @__PURE__ */ function() {
    function InputEmail2() {
    }
    ;
    InputEmail2.value = new InputEmail2();
    return InputEmail2;
  }();
  var InputFile = /* @__PURE__ */ function() {
    function InputFile2() {
    }
    ;
    InputFile2.value = new InputFile2();
    return InputFile2;
  }();
  var InputHidden = /* @__PURE__ */ function() {
    function InputHidden2() {
    }
    ;
    InputHidden2.value = new InputHidden2();
    return InputHidden2;
  }();
  var InputImage = /* @__PURE__ */ function() {
    function InputImage2() {
    }
    ;
    InputImage2.value = new InputImage2();
    return InputImage2;
  }();
  var InputMonth = /* @__PURE__ */ function() {
    function InputMonth2() {
    }
    ;
    InputMonth2.value = new InputMonth2();
    return InputMonth2;
  }();
  var InputNumber = /* @__PURE__ */ function() {
    function InputNumber2() {
    }
    ;
    InputNumber2.value = new InputNumber2();
    return InputNumber2;
  }();
  var InputPassword = /* @__PURE__ */ function() {
    function InputPassword2() {
    }
    ;
    InputPassword2.value = new InputPassword2();
    return InputPassword2;
  }();
  var InputRadio = /* @__PURE__ */ function() {
    function InputRadio2() {
    }
    ;
    InputRadio2.value = new InputRadio2();
    return InputRadio2;
  }();
  var InputRange = /* @__PURE__ */ function() {
    function InputRange2() {
    }
    ;
    InputRange2.value = new InputRange2();
    return InputRange2;
  }();
  var InputReset = /* @__PURE__ */ function() {
    function InputReset2() {
    }
    ;
    InputReset2.value = new InputReset2();
    return InputReset2;
  }();
  var InputSearch = /* @__PURE__ */ function() {
    function InputSearch2() {
    }
    ;
    InputSearch2.value = new InputSearch2();
    return InputSearch2;
  }();
  var InputSubmit = /* @__PURE__ */ function() {
    function InputSubmit2() {
    }
    ;
    InputSubmit2.value = new InputSubmit2();
    return InputSubmit2;
  }();
  var InputTel = /* @__PURE__ */ function() {
    function InputTel2() {
    }
    ;
    InputTel2.value = new InputTel2();
    return InputTel2;
  }();
  var InputText = /* @__PURE__ */ function() {
    function InputText2() {
    }
    ;
    InputText2.value = new InputText2();
    return InputText2;
  }();
  var InputTime = /* @__PURE__ */ function() {
    function InputTime2() {
    }
    ;
    InputTime2.value = new InputTime2();
    return InputTime2;
  }();
  var InputUrl = /* @__PURE__ */ function() {
    function InputUrl2() {
    }
    ;
    InputUrl2.value = new InputUrl2();
    return InputUrl2;
  }();
  var InputWeek = /* @__PURE__ */ function() {
    function InputWeek2() {
    }
    ;
    InputWeek2.value = new InputWeek2();
    return InputWeek2;
  }();
  var renderInputType = function(v) {
    if (v instanceof InputButton) {
      return "button";
    }
    ;
    if (v instanceof InputCheckbox) {
      return "checkbox";
    }
    ;
    if (v instanceof InputColor) {
      return "color";
    }
    ;
    if (v instanceof InputDate) {
      return "date";
    }
    ;
    if (v instanceof InputDatetimeLocal) {
      return "datetime-local";
    }
    ;
    if (v instanceof InputEmail) {
      return "email";
    }
    ;
    if (v instanceof InputFile) {
      return "file";
    }
    ;
    if (v instanceof InputHidden) {
      return "hidden";
    }
    ;
    if (v instanceof InputImage) {
      return "image";
    }
    ;
    if (v instanceof InputMonth) {
      return "month";
    }
    ;
    if (v instanceof InputNumber) {
      return "number";
    }
    ;
    if (v instanceof InputPassword) {
      return "password";
    }
    ;
    if (v instanceof InputRadio) {
      return "radio";
    }
    ;
    if (v instanceof InputRange) {
      return "range";
    }
    ;
    if (v instanceof InputReset) {
      return "reset";
    }
    ;
    if (v instanceof InputSearch) {
      return "search";
    }
    ;
    if (v instanceof InputSubmit) {
      return "submit";
    }
    ;
    if (v instanceof InputTel) {
      return "tel";
    }
    ;
    if (v instanceof InputText) {
      return "text";
    }
    ;
    if (v instanceof InputTime) {
      return "time";
    }
    ;
    if (v instanceof InputUrl) {
      return "url";
    }
    ;
    if (v instanceof InputWeek) {
      return "week";
    }
    ;
    throw new Error("Failed pattern match at DOM.HTML.Indexed.InputType (line 33, column 19 - line 55, column 22): " + [v.constructor.name]);
  };

  // output/Halogen.Query.Input/index.js
  var RefUpdate = /* @__PURE__ */ function() {
    function RefUpdate2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    RefUpdate2.create = function(value0) {
      return function(value1) {
        return new RefUpdate2(value0, value1);
      };
    };
    return RefUpdate2;
  }();
  var Action = /* @__PURE__ */ function() {
    function Action3(value0) {
      this.value0 = value0;
    }
    ;
    Action3.create = function(value0) {
      return new Action3(value0);
    };
    return Action3;
  }();

  // output/Data.Array/foreign.js
  var replicateFill = function(count) {
    return function(value15) {
      if (count < 1) {
        return [];
      }
      var result = new Array(count);
      return result.fill(value15);
    };
  };
  var replicatePolyfill = function(count) {
    return function(value15) {
      var result = [];
      var n = 0;
      for (var i2 = 0; i2 < count; i2++) {
        result[n++] = value15;
      }
      return result;
    };
  };
  var replicate2 = typeof Array.prototype.fill === "function" ? replicateFill : replicatePolyfill;
  var fromFoldableImpl = function() {
    function Cons3(head6, tail2) {
      this.head = head6;
      this.tail = tail2;
    }
    var emptyList = {};
    function curryCons(head6) {
      return function(tail2) {
        return new Cons3(head6, tail2);
      };
    }
    function listToArray(list) {
      var result = [];
      var count = 0;
      var xs = list;
      while (xs !== emptyList) {
        result[count++] = xs.head;
        xs = xs.tail;
      }
      return result;
    }
    return function(foldr4) {
      return function(xs) {
        return listToArray(foldr4(curryCons)(emptyList)(xs));
      };
    };
  }();
  var length4 = function(xs) {
    return xs.length;
  };
  var indexImpl = function(just) {
    return function(nothing) {
      return function(xs) {
        return function(i2) {
          return i2 < 0 || i2 >= xs.length ? nothing : just(xs[i2]);
        };
      };
    };
  };
  var findIndexImpl = function(just) {
    return function(nothing) {
      return function(f) {
        return function(xs) {
          for (var i2 = 0, l = xs.length; i2 < l; i2++) {
            if (f(xs[i2]))
              return just(i2);
          }
          return nothing;
        };
      };
    };
  };
  var _deleteAt = function(just) {
    return function(nothing) {
      return function(i2) {
        return function(l) {
          if (i2 < 0 || i2 >= l.length)
            return nothing;
          var l1 = l.slice();
          l1.splice(i2, 1);
          return just(l1);
        };
      };
    };
  };
  var _updateAt = function(just) {
    return function(nothing) {
      return function(i2) {
        return function(a2) {
          return function(l) {
            if (i2 < 0 || i2 >= l.length)
              return nothing;
            var l1 = l.slice();
            l1[i2] = a2;
            return just(l1);
          };
        };
      };
    };
  };
  var reverse2 = function(l) {
    return l.slice().reverse();
  };
  var concat = function(xss) {
    if (xss.length <= 1e4) {
      return Array.prototype.concat.apply([], xss);
    }
    var result = [];
    for (var i2 = 0, l = xss.length; i2 < l; i2++) {
      var xs = xss[i2];
      for (var j = 0, m = xs.length; j < m; j++) {
        result.push(xs[j]);
      }
    }
    return result;
  };
  var filter2 = function(f) {
    return function(xs) {
      return xs.filter(f);
    };
  };
  var sortByImpl = function() {
    function mergeFromTo(compare3, fromOrdering, xs1, xs2, from4, to) {
      var mid;
      var i2;
      var j;
      var k;
      var x;
      var y;
      var c;
      mid = from4 + (to - from4 >> 1);
      if (mid - from4 > 1)
        mergeFromTo(compare3, fromOrdering, xs2, xs1, from4, mid);
      if (to - mid > 1)
        mergeFromTo(compare3, fromOrdering, xs2, xs1, mid, to);
      i2 = from4;
      j = mid;
      k = from4;
      while (i2 < mid && j < to) {
        x = xs2[i2];
        y = xs2[j];
        c = fromOrdering(compare3(x)(y));
        if (c > 0) {
          xs1[k++] = y;
          ++j;
        } else {
          xs1[k++] = x;
          ++i2;
        }
      }
      while (i2 < mid) {
        xs1[k++] = xs2[i2++];
      }
      while (j < to) {
        xs1[k++] = xs2[j++];
      }
    }
    return function(compare3) {
      return function(fromOrdering) {
        return function(xs) {
          var out;
          if (xs.length < 2)
            return xs;
          out = xs.slice(0);
          mergeFromTo(compare3, fromOrdering, out, xs.slice(0), 0, xs.length);
          return out;
        };
      };
    };
  }();
  var slice = function(s) {
    return function(e) {
      return function(l) {
        return l.slice(s, e);
      };
    };
  };
  var unsafeIndexImpl = function(xs) {
    return function(n) {
      return xs[n];
    };
  };

  // output/Data.Array.ST/foreign.js
  var pushAll = function(as) {
    return function(xs) {
      return function() {
        return xs.push.apply(xs, as);
      };
    };
  };
  var unsafeFreeze = function(xs) {
    return function() {
      return xs;
    };
  };
  function copyImpl(xs) {
    return function() {
      return xs.slice();
    };
  }
  var thaw = copyImpl;
  var sortByImpl2 = function() {
    function mergeFromTo(compare3, fromOrdering, xs1, xs2, from4, to) {
      var mid;
      var i2;
      var j;
      var k;
      var x;
      var y;
      var c;
      mid = from4 + (to - from4 >> 1);
      if (mid - from4 > 1)
        mergeFromTo(compare3, fromOrdering, xs2, xs1, from4, mid);
      if (to - mid > 1)
        mergeFromTo(compare3, fromOrdering, xs2, xs1, mid, to);
      i2 = from4;
      j = mid;
      k = from4;
      while (i2 < mid && j < to) {
        x = xs2[i2];
        y = xs2[j];
        c = fromOrdering(compare3(x)(y));
        if (c > 0) {
          xs1[k++] = y;
          ++j;
        } else {
          xs1[k++] = x;
          ++i2;
        }
      }
      while (i2 < mid) {
        xs1[k++] = xs2[i2++];
      }
      while (j < to) {
        xs1[k++] = xs2[j++];
      }
    }
    return function(compare3) {
      return function(fromOrdering) {
        return function(xs) {
          return function() {
            if (xs.length < 2)
              return xs;
            mergeFromTo(compare3, fromOrdering, xs, xs.slice(0), 0, xs.length);
            return xs;
          };
        };
      };
    };
  }();

  // output/Data.Array.ST/index.js
  var withArray = function(f) {
    return function(xs) {
      return function __do2() {
        var result = thaw(xs)();
        f(result)();
        return unsafeFreeze(result)();
      };
    };
  };
  var push = function(a2) {
    return pushAll([a2]);
  };

  // output/Data.Array/index.js
  var fromJust4 = /* @__PURE__ */ fromJust();
  var append2 = /* @__PURE__ */ append(semigroupArray);
  var updateAt = /* @__PURE__ */ function() {
    return _updateAt(Just.create)(Nothing.value);
  }();
  var unsafeIndex = function() {
    return unsafeIndexImpl;
  };
  var take = function(n) {
    return function(xs) {
      var $149 = n < 1;
      if ($149) {
        return [];
      }
      ;
      return slice(0)(n)(xs);
    };
  };
  var snoc = function(xs) {
    return function(x) {
      return withArray(push(x))(xs)();
    };
  };
  var singleton4 = function(a2) {
    return [a2];
  };
  var mapWithIndex2 = /* @__PURE__ */ mapWithIndex(functorWithIndexArray);
  var index2 = /* @__PURE__ */ function() {
    return indexImpl(Just.create)(Nothing.value);
  }();
  var modifyAt = function(i2) {
    return function(f) {
      return function(xs) {
        var go2 = function(x) {
          return updateAt(i2)(f(x))(xs);
        };
        return maybe(Nothing.value)(go2)(index2(xs)(i2));
      };
    };
  };
  var head3 = function(xs) {
    return index2(xs)(0);
  };
  var fromFoldable2 = function(dictFoldable) {
    return fromFoldableImpl(foldr(dictFoldable));
  };
  var findIndex = /* @__PURE__ */ function() {
    return findIndexImpl(Just.create)(Nothing.value);
  }();
  var elemIndex = function(dictEq) {
    var eq22 = eq(dictEq);
    return function(x) {
      return findIndex(function(v) {
        return eq22(v)(x);
      });
    };
  };
  var drop = function(n) {
    return function(xs) {
      var $173 = n < 1;
      if ($173) {
        return xs;
      }
      ;
      return slice(n)(length4(xs))(xs);
    };
  };
  var deleteAt = /* @__PURE__ */ function() {
    return _deleteAt(Just.create)(Nothing.value);
  }();
  var deleteBy = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2.length === 0) {
          return [];
        }
        ;
        return maybe(v2)(function(i2) {
          return fromJust4(deleteAt(i2)(v2));
        })(findIndex(v(v1))(v2));
      };
    };
  };
  var cons2 = function(x) {
    return function(xs) {
      return append2([x])(xs);
    };
  };
  var some = function(dictAlternative) {
    var apply1 = apply(dictAlternative.Applicative0().Apply0());
    var map32 = map(dictAlternative.Plus1().Alt0().Functor0());
    return function(dictLazy) {
      var defer4 = defer(dictLazy);
      return function(v) {
        return apply1(map32(cons2)(v))(defer4(function(v1) {
          return many(dictAlternative)(dictLazy)(v);
        }));
      };
    };
  };
  var many = function(dictAlternative) {
    var alt8 = alt(dictAlternative.Plus1().Alt0());
    var pure110 = pure(dictAlternative.Applicative0());
    return function(dictLazy) {
      return function(v) {
        return alt8(some(dictAlternative)(dictLazy)(v))(pure110([]));
      };
    };
  };
  var concatMap = /* @__PURE__ */ flip(/* @__PURE__ */ bind(bindArray));
  var mapMaybe = function(f) {
    return concatMap(function() {
      var $191 = maybe([])(singleton4);
      return function($192) {
        return $191(f($192));
      };
    }());
  };

  // output/Halogen.VDom.Machine/index.js
  var Step = /* @__PURE__ */ function() {
    function Step3(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    Step3.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new Step3(value0, value1, value22, value32);
          };
        };
      };
    };
    return Step3;
  }();
  var unStep = unsafeCoerce2;
  var step3 = function(v, a2) {
    return v.value2(v.value1, a2);
  };
  var mkStep = unsafeCoerce2;
  var halt = function(v) {
    return v.value3(v.value1);
  };
  var extract2 = /* @__PURE__ */ unStep(function(v) {
    return v.value0;
  });

  // output/Halogen.VDom.Types/index.js
  var map9 = /* @__PURE__ */ map(functorArray);
  var map12 = /* @__PURE__ */ map(functorTuple);
  var Text = /* @__PURE__ */ function() {
    function Text2(value0) {
      this.value0 = value0;
    }
    ;
    Text2.create = function(value0) {
      return new Text2(value0);
    };
    return Text2;
  }();
  var Elem = /* @__PURE__ */ function() {
    function Elem2(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    Elem2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new Elem2(value0, value1, value22, value32);
          };
        };
      };
    };
    return Elem2;
  }();
  var Keyed = /* @__PURE__ */ function() {
    function Keyed2(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    Keyed2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new Keyed2(value0, value1, value22, value32);
          };
        };
      };
    };
    return Keyed2;
  }();
  var Widget = /* @__PURE__ */ function() {
    function Widget2(value0) {
      this.value0 = value0;
    }
    ;
    Widget2.create = function(value0) {
      return new Widget2(value0);
    };
    return Widget2;
  }();
  var Grafted = /* @__PURE__ */ function() {
    function Grafted2(value0) {
      this.value0 = value0;
    }
    ;
    Grafted2.create = function(value0) {
      return new Grafted2(value0);
    };
    return Grafted2;
  }();
  var Graft = /* @__PURE__ */ function() {
    function Graft2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    Graft2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new Graft2(value0, value1, value22);
        };
      };
    };
    return Graft2;
  }();
  var unGraft = function(f) {
    return function($61) {
      return f($61);
    };
  };
  var graft = unsafeCoerce2;
  var bifunctorGraft = {
    bimap: function(f) {
      return function(g) {
        return unGraft(function(v) {
          return graft(new Graft(function($63) {
            return f(v.value0($63));
          }, function($64) {
            return g(v.value1($64));
          }, v.value2));
        });
      };
    }
  };
  var bimap2 = /* @__PURE__ */ bimap(bifunctorGraft);
  var runGraft = /* @__PURE__ */ unGraft(function(v) {
    var go2 = function(v2) {
      if (v2 instanceof Text) {
        return new Text(v2.value0);
      }
      ;
      if (v2 instanceof Elem) {
        return new Elem(v2.value0, v2.value1, v.value0(v2.value2), map9(go2)(v2.value3));
      }
      ;
      if (v2 instanceof Keyed) {
        return new Keyed(v2.value0, v2.value1, v.value0(v2.value2), map9(map12(go2))(v2.value3));
      }
      ;
      if (v2 instanceof Widget) {
        return new Widget(v.value1(v2.value0));
      }
      ;
      if (v2 instanceof Grafted) {
        return new Grafted(bimap2(v.value0)(v.value1)(v2.value0));
      }
      ;
      throw new Error("Failed pattern match at Halogen.VDom.Types (line 86, column 7 - line 86, column 27): " + [v2.constructor.name]);
    };
    return go2(v.value2);
  });

  // output/Halogen.VDom.Util/foreign.js
  function unsafeGetAny(key, obj) {
    return obj[key];
  }
  function unsafeHasAny(key, obj) {
    return obj.hasOwnProperty(key);
  }
  function unsafeSetAny(key, val, obj) {
    obj[key] = val;
  }
  function forE2(a2, f) {
    var b2 = [];
    for (var i2 = 0; i2 < a2.length; i2++) {
      b2.push(f(i2, a2[i2]));
    }
    return b2;
  }
  function forEachE(a2, f) {
    for (var i2 = 0; i2 < a2.length; i2++) {
      f(a2[i2]);
    }
  }
  function forInE(o, f) {
    var ks = Object.keys(o);
    for (var i2 = 0; i2 < ks.length; i2++) {
      var k = ks[i2];
      f(k, o[k]);
    }
  }
  function diffWithIxE(a1, a2, f1, f2, f3) {
    var a3 = [];
    var l1 = a1.length;
    var l2 = a2.length;
    var i2 = 0;
    while (1) {
      if (i2 < l1) {
        if (i2 < l2) {
          a3.push(f1(i2, a1[i2], a2[i2]));
        } else {
          f2(i2, a1[i2]);
        }
      } else if (i2 < l2) {
        a3.push(f3(i2, a2[i2]));
      } else {
        break;
      }
      i2++;
    }
    return a3;
  }
  function strMapWithIxE(as, fk, f) {
    var o = {};
    for (var i2 = 0; i2 < as.length; i2++) {
      var a2 = as[i2];
      var k = fk(a2);
      o[k] = f(k, i2, a2);
    }
    return o;
  }
  function diffWithKeyAndIxE(o1, as, fk, f1, f2, f3) {
    var o2 = {};
    for (var i2 = 0; i2 < as.length; i2++) {
      var a2 = as[i2];
      var k = fk(a2);
      if (o1.hasOwnProperty(k)) {
        o2[k] = f1(k, i2, o1[k], a2);
      } else {
        o2[k] = f3(k, i2, a2);
      }
    }
    for (var k in o1) {
      if (k in o2) {
        continue;
      }
      f2(k, o1[k]);
    }
    return o2;
  }
  function refEq2(a2, b2) {
    return a2 === b2;
  }
  function createTextNode(s, doc) {
    return doc.createTextNode(s);
  }
  function setTextContent(s, n) {
    n.textContent = s;
  }
  function createElement(ns, name15, doc) {
    if (ns != null) {
      return doc.createElementNS(ns, name15);
    } else {
      return doc.createElement(name15);
    }
  }
  function insertChildIx(i2, a2, b2) {
    var n = b2.childNodes.item(i2) || null;
    if (n !== a2) {
      b2.insertBefore(a2, n);
    }
  }
  function removeChild(a2, b2) {
    if (b2 && a2.parentNode === b2) {
      b2.removeChild(a2);
    }
  }
  function parentNode(a2) {
    return a2.parentNode;
  }
  function setAttribute(ns, attr3, val, el) {
    if (ns != null) {
      el.setAttributeNS(ns, attr3, val);
    } else {
      el.setAttribute(attr3, val);
    }
  }
  function removeAttribute(ns, attr3, el) {
    if (ns != null) {
      el.removeAttributeNS(ns, attr3);
    } else {
      el.removeAttribute(attr3);
    }
  }
  function hasAttribute(ns, attr3, el) {
    if (ns != null) {
      return el.hasAttributeNS(ns, attr3);
    } else {
      return el.hasAttribute(attr3);
    }
  }
  function addEventListener2(ev, listener, el) {
    el.addEventListener(ev, listener, false);
  }
  function removeEventListener2(ev, listener, el) {
    el.removeEventListener(ev, listener, false);
  }
  var jsUndefined = void 0;

  // output/Foreign.Object.ST/foreign.js
  var newImpl = function() {
    return {};
  };

  // output/Halogen.VDom.Util/index.js
  var unsafeLookup = unsafeGetAny;
  var unsafeFreeze2 = unsafeCoerce2;
  var pokeMutMap = unsafeSetAny;
  var newMutMap = newImpl;

  // output/Web.DOM.Element/foreign.js
  var getProp = function(name15) {
    return function(doctype) {
      return doctype[name15];
    };
  };
  var _namespaceURI = getProp("namespaceURI");
  var _prefix = getProp("prefix");
  var localName = getProp("localName");
  var tagName = getProp("tagName");

  // output/Web.DOM.Element/index.js
  var toNode2 = unsafeCoerce2;

  // output/Halogen.VDom.DOM/index.js
  var $runtime_lazy3 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var haltWidget = function(v) {
    return halt(v.widget);
  };
  var $lazy_patchWidget = /* @__PURE__ */ $runtime_lazy3("patchWidget", "Halogen.VDom.DOM", function() {
    return function(state3, vdom) {
      if (vdom instanceof Grafted) {
        return $lazy_patchWidget(291)(state3, runGraft(vdom.value0));
      }
      ;
      if (vdom instanceof Widget) {
        var res = step3(state3.widget, vdom.value0);
        var res$prime = unStep(function(v) {
          return mkStep(new Step(v.value0, {
            build: state3.build,
            widget: res
          }, $lazy_patchWidget(296), haltWidget));
        })(res);
        return res$prime;
      }
      ;
      haltWidget(state3);
      return state3.build(vdom);
    };
  });
  var patchWidget = /* @__PURE__ */ $lazy_patchWidget(286);
  var haltText = function(v) {
    var parent2 = parentNode(v.node);
    return removeChild(v.node, parent2);
  };
  var $lazy_patchText = /* @__PURE__ */ $runtime_lazy3("patchText", "Halogen.VDom.DOM", function() {
    return function(state3, vdom) {
      if (vdom instanceof Grafted) {
        return $lazy_patchText(82)(state3, runGraft(vdom.value0));
      }
      ;
      if (vdom instanceof Text) {
        if (state3.value === vdom.value0) {
          return mkStep(new Step(state3.node, state3, $lazy_patchText(85), haltText));
        }
        ;
        if (otherwise) {
          var nextState = {
            build: state3.build,
            node: state3.node,
            value: vdom.value0
          };
          setTextContent(vdom.value0, state3.node);
          return mkStep(new Step(state3.node, nextState, $lazy_patchText(89), haltText));
        }
        ;
      }
      ;
      haltText(state3);
      return state3.build(vdom);
    };
  });
  var patchText = /* @__PURE__ */ $lazy_patchText(77);
  var haltKeyed = function(v) {
    var parent2 = parentNode(v.node);
    removeChild(v.node, parent2);
    forInE(v.children, function(v1, s) {
      return halt(s);
    });
    return halt(v.attrs);
  };
  var haltElem = function(v) {
    var parent2 = parentNode(v.node);
    removeChild(v.node, parent2);
    forEachE(v.children, halt);
    return halt(v.attrs);
  };
  var eqElemSpec = function(ns1, v, ns2, v1) {
    var $63 = v === v1;
    if ($63) {
      if (ns1 instanceof Just && (ns2 instanceof Just && ns1.value0 === ns2.value0)) {
        return true;
      }
      ;
      if (ns1 instanceof Nothing && ns2 instanceof Nothing) {
        return true;
      }
      ;
      return false;
    }
    ;
    return false;
  };
  var $lazy_patchElem = /* @__PURE__ */ $runtime_lazy3("patchElem", "Halogen.VDom.DOM", function() {
    return function(state3, vdom) {
      if (vdom instanceof Grafted) {
        return $lazy_patchElem(135)(state3, runGraft(vdom.value0));
      }
      ;
      if (vdom instanceof Elem && eqElemSpec(state3.ns, state3.name, vdom.value0, vdom.value1)) {
        var v = length4(vdom.value3);
        var v1 = length4(state3.children);
        if (v1 === 0 && v === 0) {
          var attrs2 = step3(state3.attrs, vdom.value2);
          var nextState = {
            build: state3.build,
            node: state3.node,
            attrs: attrs2,
            ns: vdom.value0,
            name: vdom.value1,
            children: state3.children
          };
          return mkStep(new Step(state3.node, nextState, $lazy_patchElem(149), haltElem));
        }
        ;
        var onThis = function(v2, s) {
          return halt(s);
        };
        var onThese = function(ix, s, v2) {
          var res = step3(s, v2);
          insertChildIx(ix, extract2(res), state3.node);
          return res;
        };
        var onThat = function(ix, v2) {
          var res = state3.build(v2);
          insertChildIx(ix, extract2(res), state3.node);
          return res;
        };
        var children2 = diffWithIxE(state3.children, vdom.value3, onThese, onThis, onThat);
        var attrs2 = step3(state3.attrs, vdom.value2);
        var nextState = {
          build: state3.build,
          node: state3.node,
          attrs: attrs2,
          ns: vdom.value0,
          name: vdom.value1,
          children: children2
        };
        return mkStep(new Step(state3.node, nextState, $lazy_patchElem(172), haltElem));
      }
      ;
      haltElem(state3);
      return state3.build(vdom);
    };
  });
  var patchElem = /* @__PURE__ */ $lazy_patchElem(130);
  var $lazy_patchKeyed = /* @__PURE__ */ $runtime_lazy3("patchKeyed", "Halogen.VDom.DOM", function() {
    return function(state3, vdom) {
      if (vdom instanceof Grafted) {
        return $lazy_patchKeyed(222)(state3, runGraft(vdom.value0));
      }
      ;
      if (vdom instanceof Keyed && eqElemSpec(state3.ns, state3.name, vdom.value0, vdom.value1)) {
        var v = length4(vdom.value3);
        if (state3.length === 0 && v === 0) {
          var attrs2 = step3(state3.attrs, vdom.value2);
          var nextState = {
            build: state3.build,
            node: state3.node,
            attrs: attrs2,
            ns: vdom.value0,
            name: vdom.value1,
            children: state3.children,
            length: 0
          };
          return mkStep(new Step(state3.node, nextState, $lazy_patchKeyed(237), haltKeyed));
        }
        ;
        var onThis = function(v2, s) {
          return halt(s);
        };
        var onThese = function(v2, ix$prime, s, v3) {
          var res = step3(s, v3.value1);
          insertChildIx(ix$prime, extract2(res), state3.node);
          return res;
        };
        var onThat = function(v2, ix, v3) {
          var res = state3.build(v3.value1);
          insertChildIx(ix, extract2(res), state3.node);
          return res;
        };
        var children2 = diffWithKeyAndIxE(state3.children, vdom.value3, fst, onThese, onThis, onThat);
        var attrs2 = step3(state3.attrs, vdom.value2);
        var nextState = {
          build: state3.build,
          node: state3.node,
          attrs: attrs2,
          ns: vdom.value0,
          name: vdom.value1,
          children: children2,
          length: v
        };
        return mkStep(new Step(state3.node, nextState, $lazy_patchKeyed(261), haltKeyed));
      }
      ;
      haltKeyed(state3);
      return state3.build(vdom);
    };
  });
  var patchKeyed = /* @__PURE__ */ $lazy_patchKeyed(217);
  var buildWidget = function(v, build, w) {
    var res = v.buildWidget(v)(w);
    var res$prime = unStep(function(v1) {
      return mkStep(new Step(v1.value0, {
        build,
        widget: res
      }, patchWidget, haltWidget));
    })(res);
    return res$prime;
  };
  var buildText = function(v, build, s) {
    var node = createTextNode(s, v.document);
    var state3 = {
      build,
      node,
      value: s
    };
    return mkStep(new Step(node, state3, patchText, haltText));
  };
  var buildKeyed = function(v, build, ns1, name1, as1, ch1) {
    var el = createElement(toNullable(ns1), name1, v.document);
    var node = toNode2(el);
    var onChild = function(v1, ix, v2) {
      var res = build(v2.value1);
      insertChildIx(ix, extract2(res), node);
      return res;
    };
    var children2 = strMapWithIxE(ch1, fst, onChild);
    var attrs = v.buildAttributes(el)(as1);
    var state3 = {
      build,
      node,
      attrs,
      ns: ns1,
      name: name1,
      children: children2,
      length: length4(ch1)
    };
    return mkStep(new Step(node, state3, patchKeyed, haltKeyed));
  };
  var buildElem = function(v, build, ns1, name1, as1, ch1) {
    var el = createElement(toNullable(ns1), name1, v.document);
    var node = toNode2(el);
    var onChild = function(ix, child) {
      var res = build(child);
      insertChildIx(ix, extract2(res), node);
      return res;
    };
    var children2 = forE2(ch1, onChild);
    var attrs = v.buildAttributes(el)(as1);
    var state3 = {
      build,
      node,
      attrs,
      ns: ns1,
      name: name1,
      children: children2
    };
    return mkStep(new Step(node, state3, patchElem, haltElem));
  };
  var buildVDom = function(spec) {
    var $lazy_build = $runtime_lazy3("build", "Halogen.VDom.DOM", function() {
      return function(v) {
        if (v instanceof Text) {
          return buildText(spec, $lazy_build(59), v.value0);
        }
        ;
        if (v instanceof Elem) {
          return buildElem(spec, $lazy_build(60), v.value0, v.value1, v.value2, v.value3);
        }
        ;
        if (v instanceof Keyed) {
          return buildKeyed(spec, $lazy_build(61), v.value0, v.value1, v.value2, v.value3);
        }
        ;
        if (v instanceof Widget) {
          return buildWidget(spec, $lazy_build(62), v.value0);
        }
        ;
        if (v instanceof Grafted) {
          return $lazy_build(63)(runGraft(v.value0));
        }
        ;
        throw new Error("Failed pattern match at Halogen.VDom.DOM (line 58, column 27 - line 63, column 52): " + [v.constructor.name]);
      };
    });
    var build = $lazy_build(58);
    return build;
  };

  // output/Foreign/foreign.js
  function typeOf(value15) {
    return typeof value15;
  }
  function tagOf(value15) {
    return Object.prototype.toString.call(value15).slice(8, -1);
  }
  var isArray = Array.isArray || function(value15) {
    return Object.prototype.toString.call(value15) === "[object Array]";
  };

  // output/Data.Int/foreign.js
  var fromNumberImpl = function(just) {
    return function(nothing) {
      return function(n) {
        return (n | 0) === n ? just(n) : nothing;
      };
    };
  };
  var toNumber = function(n) {
    return n;
  };
  var fromStringAsImpl = function(just) {
    return function(nothing) {
      return function(radix) {
        var digits;
        if (radix < 11) {
          digits = "[0-" + (radix - 1).toString() + "]";
        } else if (radix === 11) {
          digits = "[0-9a]";
        } else {
          digits = "[0-9a-" + String.fromCharCode(86 + radix) + "]";
        }
        var pattern2 = new RegExp("^[\\+\\-]?" + digits + "+$", "i");
        return function(s) {
          if (pattern2.test(s)) {
            var i2 = parseInt(s, radix);
            return (i2 | 0) === i2 ? just(i2) : nothing;
          } else {
            return nothing;
          }
        };
      };
    };
  };
  var toStringAs = function(radix) {
    return function(i2) {
      return i2.toString(radix);
    };
  };

  // output/Data.Number/foreign.js
  var isFiniteImpl = isFinite;
  var floor = Math.floor;

  // output/Data.Int/index.js
  var top2 = /* @__PURE__ */ top(boundedInt);
  var bottom2 = /* @__PURE__ */ bottom(boundedInt);
  var hexadecimal = 16;
  var fromStringAs = /* @__PURE__ */ function() {
    return fromStringAsImpl(Just.create)(Nothing.value);
  }();
  var fromNumber = /* @__PURE__ */ function() {
    return fromNumberImpl(Just.create)(Nothing.value);
  }();
  var unsafeClamp = function(x) {
    if (!isFiniteImpl(x)) {
      return 0;
    }
    ;
    if (x >= toNumber(top2)) {
      return top2;
    }
    ;
    if (x <= toNumber(bottom2)) {
      return bottom2;
    }
    ;
    if (otherwise) {
      return fromMaybe(0)(fromNumber(x));
    }
    ;
    throw new Error("Failed pattern match at Data.Int (line 72, column 1 - line 72, column 29): " + [x.constructor.name]);
  };
  var floor2 = function($39) {
    return unsafeClamp(floor($39));
  };
  var decimal = 10;

  // output/Data.List.NonEmpty/index.js
  var singleton5 = /* @__PURE__ */ function() {
    var $200 = singleton2(plusList);
    return function($201) {
      return NonEmptyList($200($201));
    };
  }();
  var cons3 = function(y) {
    return function(v) {
      return new NonEmpty(y, new Cons(v.value0, v.value1));
    };
  };

  // output/Data.String.CodeUnits/foreign.js
  var fromCharArray = function(a2) {
    return a2.join("");
  };
  var toCharArray = function(s) {
    return s.split("");
  };
  var singleton6 = function(c) {
    return c;
  };
  var length6 = function(s) {
    return s.length;
  };
  var drop3 = function(n) {
    return function(s) {
      return s.substring(n);
    };
  };
  var splitAt = function(i2) {
    return function(s) {
      return { before: s.substring(0, i2), after: s.substring(i2) };
    };
  };

  // output/Data.String.Unsafe/foreign.js
  var charAt = function(i2) {
    return function(s) {
      if (i2 >= 0 && i2 < s.length)
        return s.charAt(i2);
      throw new Error("Data.String.Unsafe.charAt: Invalid index.");
    };
  };
  var char = function(s) {
    if (s.length === 1)
      return s.charAt(0);
    throw new Error("Data.String.Unsafe.char: Expected string of length 1.");
  };

  // output/Data.String.CodeUnits/index.js
  var stripPrefix = function(v) {
    return function(str) {
      var v1 = splitAt(length6(v))(str);
      var $20 = v1.before === v;
      if ($20) {
        return new Just(v1.after);
      }
      ;
      return Nothing.value;
    };
  };

  // output/Foreign/index.js
  var TypeMismatch = /* @__PURE__ */ function() {
    function TypeMismatch2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    TypeMismatch2.create = function(value0) {
      return function(value1) {
        return new TypeMismatch2(value0, value1);
      };
    };
    return TypeMismatch2;
  }();
  var unsafeToForeign = unsafeCoerce2;
  var unsafeFromForeign = unsafeCoerce2;
  var fail = function(dictMonad) {
    var $153 = throwError(monadThrowExceptT(dictMonad));
    return function($154) {
      return $153(singleton5($154));
    };
  };
  var unsafeReadTagged = function(dictMonad) {
    var pure110 = pure(applicativeExceptT(dictMonad));
    var fail1 = fail(dictMonad);
    return function(tag) {
      return function(value15) {
        if (tagOf(value15) === tag) {
          return pure110(unsafeFromForeign(value15));
        }
        ;
        if (otherwise) {
          return fail1(new TypeMismatch(tag, tagOf(value15)));
        }
        ;
        throw new Error("Failed pattern match at Foreign (line 123, column 1 - line 123, column 104): " + [tag.constructor.name, value15.constructor.name]);
      };
    };
  };
  var readString = function(dictMonad) {
    return unsafeReadTagged(dictMonad)("String");
  };

  // output/Foreign.Object/foreign.js
  function _lookup(no, yes, k, m) {
    return k in m ? yes(m[k]) : no;
  }
  function toArrayWithKey(f) {
    return function(m) {
      var r = [];
      for (var k in m) {
        if (hasOwnProperty.call(m, k)) {
          r.push(f(k)(m[k]));
        }
      }
      return r;
    };
  }
  var keys = Object.keys || toArrayWithKey(function(k) {
    return function() {
      return k;
    };
  });

  // output/Data.Function.Uncurried/foreign.js
  var mkFn2 = function(fn) {
    return function(a2, b2) {
      return fn(a2)(b2);
    };
  };
  var mkFn5 = function(fn) {
    return function(a2, b2, c, d, e) {
      return fn(a2)(b2)(c)(d)(e);
    };
  };
  var runFn4 = function(fn) {
    return function(a2) {
      return function(b2) {
        return function(c) {
          return function(d) {
            return fn(a2, b2, c, d);
          };
        };
      };
    };
  };

  // output/Foreign.Object/index.js
  var lookup3 = /* @__PURE__ */ function() {
    return runFn4(_lookup)(Nothing.value)(Just.create);
  }();

  // output/Halogen.VDom.DOM.Prop/index.js
  var $runtime_lazy4 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var Created = /* @__PURE__ */ function() {
    function Created2(value0) {
      this.value0 = value0;
    }
    ;
    Created2.create = function(value0) {
      return new Created2(value0);
    };
    return Created2;
  }();
  var Removed = /* @__PURE__ */ function() {
    function Removed2(value0) {
      this.value0 = value0;
    }
    ;
    Removed2.create = function(value0) {
      return new Removed2(value0);
    };
    return Removed2;
  }();
  var Attribute = /* @__PURE__ */ function() {
    function Attribute2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    Attribute2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new Attribute2(value0, value1, value22);
        };
      };
    };
    return Attribute2;
  }();
  var Property = /* @__PURE__ */ function() {
    function Property2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Property2.create = function(value0) {
      return function(value1) {
        return new Property2(value0, value1);
      };
    };
    return Property2;
  }();
  var Handler = /* @__PURE__ */ function() {
    function Handler2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Handler2.create = function(value0) {
      return function(value1) {
        return new Handler2(value0, value1);
      };
    };
    return Handler2;
  }();
  var Ref = /* @__PURE__ */ function() {
    function Ref2(value0) {
      this.value0 = value0;
    }
    ;
    Ref2.create = function(value0) {
      return new Ref2(value0);
    };
    return Ref2;
  }();
  var unsafeGetProperty = unsafeGetAny;
  var setProperty = unsafeSetAny;
  var removeProperty = function(key, el) {
    var v = hasAttribute(nullImpl, key, el);
    if (v) {
      return removeAttribute(nullImpl, key, el);
    }
    ;
    var v1 = typeOf(unsafeGetAny(key, el));
    if (v1 === "string") {
      return unsafeSetAny(key, "", el);
    }
    ;
    if (key === "rowSpan") {
      return unsafeSetAny(key, 1, el);
    }
    ;
    if (key === "colSpan") {
      return unsafeSetAny(key, 1, el);
    }
    ;
    return unsafeSetAny(key, jsUndefined, el);
  };
  var propToStrKey = function(v) {
    if (v instanceof Attribute && v.value0 instanceof Just) {
      return "attr/" + (v.value0.value0 + (":" + v.value1));
    }
    ;
    if (v instanceof Attribute) {
      return "attr/:" + v.value1;
    }
    ;
    if (v instanceof Property) {
      return "prop/" + v.value0;
    }
    ;
    if (v instanceof Handler) {
      return "handler/" + v.value0;
    }
    ;
    if (v instanceof Ref) {
      return "ref";
    }
    ;
    throw new Error("Failed pattern match at Halogen.VDom.DOM.Prop (line 182, column 16 - line 187, column 16): " + [v.constructor.name]);
  };
  var propFromString = unsafeCoerce2;
  var propFromBoolean = unsafeCoerce2;
  var buildProp = function(emit) {
    return function(el) {
      var removeProp = function(prevEvents) {
        return function(v, v1) {
          if (v1 instanceof Attribute) {
            return removeAttribute(toNullable(v1.value0), v1.value1, el);
          }
          ;
          if (v1 instanceof Property) {
            return removeProperty(v1.value0, el);
          }
          ;
          if (v1 instanceof Handler) {
            var handler3 = unsafeLookup(v1.value0, prevEvents);
            return removeEventListener2(v1.value0, fst(handler3), el);
          }
          ;
          if (v1 instanceof Ref) {
            return unit;
          }
          ;
          throw new Error("Failed pattern match at Halogen.VDom.DOM.Prop (line 169, column 5 - line 179, column 18): " + [v1.constructor.name]);
        };
      };
      var mbEmit = function(v) {
        if (v instanceof Just) {
          return emit(v.value0)();
        }
        ;
        return unit;
      };
      var haltProp = function(state3) {
        var v = lookup3("ref")(state3.props);
        if (v instanceof Just && v.value0 instanceof Ref) {
          return mbEmit(v.value0.value0(new Removed(el)));
        }
        ;
        return unit;
      };
      var diffProp = function(prevEvents, events) {
        return function(v, v1, v11, v2) {
          if (v11 instanceof Attribute && v2 instanceof Attribute) {
            var $66 = v11.value2 === v2.value2;
            if ($66) {
              return v2;
            }
            ;
            setAttribute(toNullable(v2.value0), v2.value1, v2.value2, el);
            return v2;
          }
          ;
          if (v11 instanceof Property && v2 instanceof Property) {
            var v4 = refEq2(v11.value1, v2.value1);
            if (v4) {
              return v2;
            }
            ;
            if (v2.value0 === "value") {
              var elVal = unsafeGetProperty("value", el);
              var $75 = refEq2(elVal, v2.value1);
              if ($75) {
                return v2;
              }
              ;
              setProperty(v2.value0, v2.value1, el);
              return v2;
            }
            ;
            setProperty(v2.value0, v2.value1, el);
            return v2;
          }
          ;
          if (v11 instanceof Handler && v2 instanceof Handler) {
            var handler3 = unsafeLookup(v2.value0, prevEvents);
            write(v2.value1)(snd(handler3))();
            pokeMutMap(v2.value0, handler3, events);
            return v2;
          }
          ;
          return v2;
        };
      };
      var applyProp = function(events) {
        return function(v, v1, v2) {
          if (v2 instanceof Attribute) {
            setAttribute(toNullable(v2.value0), v2.value1, v2.value2, el);
            return v2;
          }
          ;
          if (v2 instanceof Property) {
            setProperty(v2.value0, v2.value1, el);
            return v2;
          }
          ;
          if (v2 instanceof Handler) {
            var v3 = unsafeGetAny(v2.value0, events);
            if (unsafeHasAny(v2.value0, events)) {
              write(v2.value1)(snd(v3))();
              return v2;
            }
            ;
            var ref2 = $$new(v2.value1)();
            var listener = eventListener(function(ev) {
              return function __do2() {
                var f$prime = read(ref2)();
                return mbEmit(f$prime(ev));
              };
            })();
            pokeMutMap(v2.value0, new Tuple(listener, ref2), events);
            addEventListener2(v2.value0, listener, el);
            return v2;
          }
          ;
          if (v2 instanceof Ref) {
            mbEmit(v2.value0(new Created(el)));
            return v2;
          }
          ;
          throw new Error("Failed pattern match at Halogen.VDom.DOM.Prop (line 113, column 5 - line 135, column 15): " + [v2.constructor.name]);
        };
      };
      var $lazy_patchProp = $runtime_lazy4("patchProp", "Halogen.VDom.DOM.Prop", function() {
        return function(state3, ps2) {
          var events = newMutMap();
          var onThis = removeProp(state3.events);
          var onThese = diffProp(state3.events, events);
          var onThat = applyProp(events);
          var props = diffWithKeyAndIxE(state3.props, ps2, propToStrKey, onThese, onThis, onThat);
          var nextState = {
            events: unsafeFreeze2(events),
            props
          };
          return mkStep(new Step(unit, nextState, $lazy_patchProp(100), haltProp));
        };
      });
      var patchProp = $lazy_patchProp(87);
      var renderProp = function(ps1) {
        var events = newMutMap();
        var ps1$prime = strMapWithIxE(ps1, propToStrKey, applyProp(events));
        var state3 = {
          events: unsafeFreeze2(events),
          props: ps1$prime
        };
        return mkStep(new Step(unit, state3, patchProp, haltProp));
      };
      return renderProp;
    };
  };

  // output/Halogen.HTML.Core/index.js
  var HTML = function(x) {
    return x;
  };
  var widget = function($28) {
    return HTML(Widget.create($28));
  };
  var toPropValue = function(dict) {
    return dict.toPropValue;
  };
  var text5 = function($29) {
    return HTML(Text.create($29));
  };
  var prop = function(dictIsProp) {
    var toPropValue1 = toPropValue(dictIsProp);
    return function(v) {
      var $31 = Property.create(v);
      return function($32) {
        return $31(toPropValue1($32));
      };
    };
  };
  var isPropString = {
    toPropValue: propFromString
  };
  var isPropInputType = {
    toPropValue: function($45) {
      return propFromString(renderInputType($45));
    }
  };
  var isPropButtonType = {
    toPropValue: function($50) {
      return propFromString(renderButtonType($50));
    }
  };
  var isPropBoolean = {
    toPropValue: propFromBoolean
  };
  var handler = /* @__PURE__ */ function() {
    return Handler.create;
  }();
  var element = function(ns) {
    return function(name15) {
      return function(props) {
        return function(children2) {
          return new Elem(ns, name15, props, children2);
        };
      };
    };
  };
  var attr = function(ns) {
    return function(v) {
      return Attribute.create(ns)(v);
    };
  };

  // output/Control.Applicative.Free/index.js
  var identity7 = /* @__PURE__ */ identity(categoryFn);
  var Pure = /* @__PURE__ */ function() {
    function Pure2(value0) {
      this.value0 = value0;
    }
    ;
    Pure2.create = function(value0) {
      return new Pure2(value0);
    };
    return Pure2;
  }();
  var Lift = /* @__PURE__ */ function() {
    function Lift4(value0) {
      this.value0 = value0;
    }
    ;
    Lift4.create = function(value0) {
      return new Lift4(value0);
    };
    return Lift4;
  }();
  var Ap = /* @__PURE__ */ function() {
    function Ap2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Ap2.create = function(value0) {
      return function(value1) {
        return new Ap2(value0, value1);
      };
    };
    return Ap2;
  }();
  var mkAp = function(fba) {
    return function(fb) {
      return new Ap(fba, fb);
    };
  };
  var liftFreeAp = /* @__PURE__ */ function() {
    return Lift.create;
  }();
  var goLeft = function(dictApplicative) {
    var pure21 = pure(dictApplicative);
    return function(fStack) {
      return function(valStack) {
        return function(nat) {
          return function(func) {
            return function(count) {
              if (func instanceof Pure) {
                return new Tuple(new Cons({
                  func: pure21(func.value0),
                  count
                }, fStack), valStack);
              }
              ;
              if (func instanceof Lift) {
                return new Tuple(new Cons({
                  func: nat(func.value0),
                  count
                }, fStack), valStack);
              }
              ;
              if (func instanceof Ap) {
                return goLeft(dictApplicative)(fStack)(cons3(func.value1)(valStack))(nat)(func.value0)(count + 1 | 0);
              }
              ;
              throw new Error("Failed pattern match at Control.Applicative.Free (line 102, column 41 - line 105, column 81): " + [func.constructor.name]);
            };
          };
        };
      };
    };
  };
  var goApply = function(dictApplicative) {
    var apply4 = apply(dictApplicative.Apply0());
    return function(fStack) {
      return function(vals) {
        return function(gVal) {
          if (fStack instanceof Nil) {
            return new Left(gVal);
          }
          ;
          if (fStack instanceof Cons) {
            var gRes = apply4(fStack.value0.func)(gVal);
            var $31 = fStack.value0.count === 1;
            if ($31) {
              if (fStack.value1 instanceof Nil) {
                return new Left(gRes);
              }
              ;
              return goApply(dictApplicative)(fStack.value1)(vals)(gRes);
            }
            ;
            if (vals instanceof Nil) {
              return new Left(gRes);
            }
            ;
            if (vals instanceof Cons) {
              return new Right(new Tuple(new Cons({
                func: gRes,
                count: fStack.value0.count - 1 | 0
              }, fStack.value1), new NonEmpty(vals.value0, vals.value1)));
            }
            ;
            throw new Error("Failed pattern match at Control.Applicative.Free (line 83, column 11 - line 88, column 50): " + [vals.constructor.name]);
          }
          ;
          throw new Error("Failed pattern match at Control.Applicative.Free (line 72, column 3 - line 88, column 50): " + [fStack.constructor.name]);
        };
      };
    };
  };
  var functorFreeAp = {
    map: function(f) {
      return function(x) {
        return mkAp(new Pure(f))(x);
      };
    }
  };
  var foldFreeAp = function(dictApplicative) {
    var goApply1 = goApply(dictApplicative);
    var pure21 = pure(dictApplicative);
    var goLeft1 = goLeft(dictApplicative);
    return function(nat) {
      return function(z) {
        var go2 = function($copy_v) {
          var $tco_done = false;
          var $tco_result;
          function $tco_loop(v) {
            if (v.value1.value0 instanceof Pure) {
              var v1 = goApply1(v.value0)(v.value1.value1)(pure21(v.value1.value0.value0));
              if (v1 instanceof Left) {
                $tco_done = true;
                return v1.value0;
              }
              ;
              if (v1 instanceof Right) {
                $copy_v = v1.value0;
                return;
              }
              ;
              throw new Error("Failed pattern match at Control.Applicative.Free (line 54, column 17 - line 56, column 24): " + [v1.constructor.name]);
            }
            ;
            if (v.value1.value0 instanceof Lift) {
              var v1 = goApply1(v.value0)(v.value1.value1)(nat(v.value1.value0.value0));
              if (v1 instanceof Left) {
                $tco_done = true;
                return v1.value0;
              }
              ;
              if (v1 instanceof Right) {
                $copy_v = v1.value0;
                return;
              }
              ;
              throw new Error("Failed pattern match at Control.Applicative.Free (line 57, column 17 - line 59, column 24): " + [v1.constructor.name]);
            }
            ;
            if (v.value1.value0 instanceof Ap) {
              var nextVals = new NonEmpty(v.value1.value0.value1, v.value1.value1);
              $copy_v = goLeft1(v.value0)(nextVals)(nat)(v.value1.value0.value0)(1);
              return;
            }
            ;
            throw new Error("Failed pattern match at Control.Applicative.Free (line 53, column 5 - line 62, column 47): " + [v.value1.value0.constructor.name]);
          }
          ;
          while (!$tco_done) {
            $tco_result = $tco_loop($copy_v);
          }
          ;
          return $tco_result;
        };
        return go2(new Tuple(Nil.value, singleton5(z)));
      };
    };
  };
  var retractFreeAp = function(dictApplicative) {
    return foldFreeAp(dictApplicative)(identity7);
  };
  var applyFreeAp = {
    apply: function(fba) {
      return function(fb) {
        return mkAp(fba)(fb);
      };
    },
    Functor0: function() {
      return functorFreeAp;
    }
  };
  var applicativeFreeAp = /* @__PURE__ */ function() {
    return {
      pure: Pure.create,
      Apply0: function() {
        return applyFreeAp;
      }
    };
  }();
  var foldFreeAp1 = /* @__PURE__ */ foldFreeAp(applicativeFreeAp);
  var hoistFreeAp = function(f) {
    return foldFreeAp1(function($54) {
      return liftFreeAp(f($54));
    });
  };

  // output/Data.CatQueue/index.js
  var CatQueue = /* @__PURE__ */ function() {
    function CatQueue2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    CatQueue2.create = function(value0) {
      return function(value1) {
        return new CatQueue2(value0, value1);
      };
    };
    return CatQueue2;
  }();
  var uncons2 = function($copy_v) {
    var $tco_done = false;
    var $tco_result;
    function $tco_loop(v) {
      if (v.value0 instanceof Nil && v.value1 instanceof Nil) {
        $tco_done = true;
        return Nothing.value;
      }
      ;
      if (v.value0 instanceof Nil) {
        $copy_v = new CatQueue(reverse(v.value1), Nil.value);
        return;
      }
      ;
      if (v.value0 instanceof Cons) {
        $tco_done = true;
        return new Just(new Tuple(v.value0.value0, new CatQueue(v.value0.value1, v.value1)));
      }
      ;
      throw new Error("Failed pattern match at Data.CatQueue (line 82, column 1 - line 82, column 63): " + [v.constructor.name]);
    }
    ;
    while (!$tco_done) {
      $tco_result = $tco_loop($copy_v);
    }
    ;
    return $tco_result;
  };
  var snoc3 = function(v) {
    return function(a2) {
      return new CatQueue(v.value0, new Cons(a2, v.value1));
    };
  };
  var $$null3 = function(v) {
    if (v.value0 instanceof Nil && v.value1 instanceof Nil) {
      return true;
    }
    ;
    return false;
  };
  var empty5 = /* @__PURE__ */ function() {
    return new CatQueue(Nil.value, Nil.value);
  }();

  // output/Data.CatList/index.js
  var CatNil = /* @__PURE__ */ function() {
    function CatNil2() {
    }
    ;
    CatNil2.value = new CatNil2();
    return CatNil2;
  }();
  var CatCons = /* @__PURE__ */ function() {
    function CatCons2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    CatCons2.create = function(value0) {
      return function(value1) {
        return new CatCons2(value0, value1);
      };
    };
    return CatCons2;
  }();
  var link = function(v) {
    return function(v1) {
      if (v instanceof CatNil) {
        return v1;
      }
      ;
      if (v1 instanceof CatNil) {
        return v;
      }
      ;
      if (v instanceof CatCons) {
        return new CatCons(v.value0, snoc3(v.value1)(v1));
      }
      ;
      throw new Error("Failed pattern match at Data.CatList (line 108, column 1 - line 108, column 54): " + [v.constructor.name, v1.constructor.name]);
    };
  };
  var foldr3 = function(k) {
    return function(b2) {
      return function(q2) {
        var foldl3 = function($copy_v) {
          return function($copy_v1) {
            return function($copy_v2) {
              var $tco_var_v = $copy_v;
              var $tco_var_v1 = $copy_v1;
              var $tco_done = false;
              var $tco_result;
              function $tco_loop(v, v1, v2) {
                if (v2 instanceof Nil) {
                  $tco_done = true;
                  return v1;
                }
                ;
                if (v2 instanceof Cons) {
                  $tco_var_v = v;
                  $tco_var_v1 = v(v1)(v2.value0);
                  $copy_v2 = v2.value1;
                  return;
                }
                ;
                throw new Error("Failed pattern match at Data.CatList (line 124, column 3 - line 124, column 59): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
              }
              ;
              while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_v, $tco_var_v1, $copy_v2);
              }
              ;
              return $tco_result;
            };
          };
        };
        var go2 = function($copy_xs) {
          return function($copy_ys) {
            var $tco_var_xs = $copy_xs;
            var $tco_done1 = false;
            var $tco_result;
            function $tco_loop(xs, ys) {
              var v = uncons2(xs);
              if (v instanceof Nothing) {
                $tco_done1 = true;
                return foldl3(function(x) {
                  return function(i2) {
                    return i2(x);
                  };
                })(b2)(ys);
              }
              ;
              if (v instanceof Just) {
                $tco_var_xs = v.value0.value1;
                $copy_ys = new Cons(k(v.value0.value0), ys);
                return;
              }
              ;
              throw new Error("Failed pattern match at Data.CatList (line 120, column 14 - line 122, column 67): " + [v.constructor.name]);
            }
            ;
            while (!$tco_done1) {
              $tco_result = $tco_loop($tco_var_xs, $copy_ys);
            }
            ;
            return $tco_result;
          };
        };
        return go2(q2)(Nil.value);
      };
    };
  };
  var uncons3 = function(v) {
    if (v instanceof CatNil) {
      return Nothing.value;
    }
    ;
    if (v instanceof CatCons) {
      return new Just(new Tuple(v.value0, function() {
        var $66 = $$null3(v.value1);
        if ($66) {
          return CatNil.value;
        }
        ;
        return foldr3(link)(CatNil.value)(v.value1);
      }()));
    }
    ;
    throw new Error("Failed pattern match at Data.CatList (line 99, column 1 - line 99, column 61): " + [v.constructor.name]);
  };
  var empty6 = /* @__PURE__ */ function() {
    return CatNil.value;
  }();
  var append3 = link;
  var semigroupCatList = {
    append: append3
  };
  var snoc4 = function(cat) {
    return function(a2) {
      return append3(cat)(new CatCons(a2, empty5));
    };
  };

  // output/Control.Monad.Free/index.js
  var $runtime_lazy5 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var append4 = /* @__PURE__ */ append(semigroupCatList);
  var Free = /* @__PURE__ */ function() {
    function Free2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Free2.create = function(value0) {
      return function(value1) {
        return new Free2(value0, value1);
      };
    };
    return Free2;
  }();
  var Return = /* @__PURE__ */ function() {
    function Return2(value0) {
      this.value0 = value0;
    }
    ;
    Return2.create = function(value0) {
      return new Return2(value0);
    };
    return Return2;
  }();
  var Bind = /* @__PURE__ */ function() {
    function Bind2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Bind2.create = function(value0) {
      return function(value1) {
        return new Bind2(value0, value1);
      };
    };
    return Bind2;
  }();
  var toView = function($copy_v) {
    var $tco_done = false;
    var $tco_result;
    function $tco_loop(v) {
      var runExpF = function(v22) {
        return v22;
      };
      var concatF = function(v22) {
        return function(r) {
          return new Free(v22.value0, append4(v22.value1)(r));
        };
      };
      if (v.value0 instanceof Return) {
        var v2 = uncons3(v.value1);
        if (v2 instanceof Nothing) {
          $tco_done = true;
          return new Return(v.value0.value0);
        }
        ;
        if (v2 instanceof Just) {
          $copy_v = concatF(runExpF(v2.value0.value0)(v.value0.value0))(v2.value0.value1);
          return;
        }
        ;
        throw new Error("Failed pattern match at Control.Monad.Free (line 227, column 7 - line 231, column 64): " + [v2.constructor.name]);
      }
      ;
      if (v.value0 instanceof Bind) {
        $tco_done = true;
        return new Bind(v.value0.value0, function(a2) {
          return concatF(v.value0.value1(a2))(v.value1);
        });
      }
      ;
      throw new Error("Failed pattern match at Control.Monad.Free (line 225, column 3 - line 233, column 56): " + [v.value0.constructor.name]);
    }
    ;
    while (!$tco_done) {
      $tco_result = $tco_loop($copy_v);
    }
    ;
    return $tco_result;
  };
  var fromView = function(f) {
    return new Free(f, empty6);
  };
  var freeMonad = {
    Applicative0: function() {
      return freeApplicative;
    },
    Bind1: function() {
      return freeBind;
    }
  };
  var freeFunctor = {
    map: function(k) {
      return function(f) {
        return bindFlipped(freeBind)(function() {
          var $189 = pure(freeApplicative);
          return function($190) {
            return $189(k($190));
          };
        }())(f);
      };
    }
  };
  var freeBind = {
    bind: function(v) {
      return function(k) {
        return new Free(v.value0, snoc4(v.value1)(k));
      };
    },
    Apply0: function() {
      return $lazy_freeApply(0);
    }
  };
  var freeApplicative = {
    pure: function($191) {
      return fromView(Return.create($191));
    },
    Apply0: function() {
      return $lazy_freeApply(0);
    }
  };
  var $lazy_freeApply = /* @__PURE__ */ $runtime_lazy5("freeApply", "Control.Monad.Free", function() {
    return {
      apply: ap(freeMonad),
      Functor0: function() {
        return freeFunctor;
      }
    };
  });
  var freeApply = /* @__PURE__ */ $lazy_freeApply(77);
  var lift22 = /* @__PURE__ */ lift2(freeApply);
  var pure4 = /* @__PURE__ */ pure(freeApplicative);
  var semigroupFree = function(dictSemigroup) {
    return {
      append: lift22(append(dictSemigroup))
    };
  };
  var liftF = function(f) {
    return fromView(new Bind(f, function($192) {
      return pure4($192);
    }));
  };
  var monoidFree = function(dictMonoid) {
    var semigroupFree1 = semigroupFree(dictMonoid.Semigroup0());
    return {
      mempty: pure4(mempty(dictMonoid)),
      Semigroup0: function() {
        return semigroupFree1;
      }
    };
  };
  var foldFree = function(dictMonadRec) {
    var Monad0 = dictMonadRec.Monad0();
    var map115 = map(Monad0.Bind1().Apply0().Functor0());
    var pure110 = pure(Monad0.Applicative0());
    var tailRecM5 = tailRecM(dictMonadRec);
    return function(k) {
      var go2 = function(f) {
        var v = toView(f);
        if (v instanceof Return) {
          return map115(Done.create)(pure110(v.value0));
        }
        ;
        if (v instanceof Bind) {
          return map115(function($199) {
            return Loop.create(v.value1($199));
          })(k(v.value0));
        }
        ;
        throw new Error("Failed pattern match at Control.Monad.Free (line 158, column 10 - line 160, column 37): " + [v.constructor.name]);
      };
      return tailRecM5(go2);
    };
  };

  // output/Halogen.Query.ChildQuery/index.js
  var ChildQuery = /* @__PURE__ */ function() {
    function ChildQuery3(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    ChildQuery3.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new ChildQuery3(value0, value1, value22);
        };
      };
    };
    return ChildQuery3;
  }();
  var unChildQueryBox = unsafeCoerce2;
  var mkChildQueryBox = unsafeCoerce2;

  // output/Unsafe.Reference/foreign.js
  function reallyUnsafeRefEq(a2) {
    return function(b2) {
      return a2 === b2;
    };
  }

  // output/Unsafe.Reference/index.js
  var unsafeRefEq = reallyUnsafeRefEq;

  // output/Halogen.Subscription/index.js
  var $$void4 = /* @__PURE__ */ $$void(functorEffect);
  var coerce3 = /* @__PURE__ */ coerce();
  var bind3 = /* @__PURE__ */ bind(bindEffect);
  var append5 = /* @__PURE__ */ append(semigroupArray);
  var traverse_2 = /* @__PURE__ */ traverse_(applicativeEffect);
  var traverse_1 = /* @__PURE__ */ traverse_2(foldableArray);
  var unsubscribe = function(v) {
    return v;
  };
  var subscribe = function(v) {
    return function(k) {
      return v(function($76) {
        return $$void4(k($76));
      });
    };
  };
  var notify = function(v) {
    return function(a2) {
      return v(a2);
    };
  };
  var makeEmitter = coerce3;
  var create3 = function __do() {
    var subscribers = $$new([])();
    return {
      emitter: function(k) {
        return function __do2() {
          modify_(function(v) {
            return append5(v)([k]);
          })(subscribers)();
          return modify_(deleteBy(unsafeRefEq)(k))(subscribers);
        };
      },
      listener: function(a2) {
        return bind3(read(subscribers))(traverse_1(function(k) {
          return k(a2);
        }));
      }
    };
  };

  // output/Halogen.Query.HalogenM/index.js
  var identity8 = /* @__PURE__ */ identity(categoryFn);
  var lookup4 = /* @__PURE__ */ lookup2();
  var SubscriptionId = function(x) {
    return x;
  };
  var ForkId = function(x) {
    return x;
  };
  var State = /* @__PURE__ */ function() {
    function State2(value0) {
      this.value0 = value0;
    }
    ;
    State2.create = function(value0) {
      return new State2(value0);
    };
    return State2;
  }();
  var Subscribe = /* @__PURE__ */ function() {
    function Subscribe2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Subscribe2.create = function(value0) {
      return function(value1) {
        return new Subscribe2(value0, value1);
      };
    };
    return Subscribe2;
  }();
  var Unsubscribe = /* @__PURE__ */ function() {
    function Unsubscribe2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Unsubscribe2.create = function(value0) {
      return function(value1) {
        return new Unsubscribe2(value0, value1);
      };
    };
    return Unsubscribe2;
  }();
  var Lift2 = /* @__PURE__ */ function() {
    function Lift4(value0) {
      this.value0 = value0;
    }
    ;
    Lift4.create = function(value0) {
      return new Lift4(value0);
    };
    return Lift4;
  }();
  var ChildQuery2 = /* @__PURE__ */ function() {
    function ChildQuery3(value0) {
      this.value0 = value0;
    }
    ;
    ChildQuery3.create = function(value0) {
      return new ChildQuery3(value0);
    };
    return ChildQuery3;
  }();
  var Raise = /* @__PURE__ */ function() {
    function Raise2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Raise2.create = function(value0) {
      return function(value1) {
        return new Raise2(value0, value1);
      };
    };
    return Raise2;
  }();
  var Par = /* @__PURE__ */ function() {
    function Par2(value0) {
      this.value0 = value0;
    }
    ;
    Par2.create = function(value0) {
      return new Par2(value0);
    };
    return Par2;
  }();
  var Fork = /* @__PURE__ */ function() {
    function Fork2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Fork2.create = function(value0) {
      return function(value1) {
        return new Fork2(value0, value1);
      };
    };
    return Fork2;
  }();
  var Join = /* @__PURE__ */ function() {
    function Join2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Join2.create = function(value0) {
      return function(value1) {
        return new Join2(value0, value1);
      };
    };
    return Join2;
  }();
  var Kill = /* @__PURE__ */ function() {
    function Kill2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Kill2.create = function(value0) {
      return function(value1) {
        return new Kill2(value0, value1);
      };
    };
    return Kill2;
  }();
  var GetRef = /* @__PURE__ */ function() {
    function GetRef2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    GetRef2.create = function(value0) {
      return function(value1) {
        return new GetRef2(value0, value1);
      };
    };
    return GetRef2;
  }();
  var HalogenM = function(x) {
    return x;
  };
  var unsubscribe2 = function(sid) {
    return liftF(new Unsubscribe(sid, unit));
  };
  var subscribe2 = function(es) {
    return liftF(new Subscribe(function(v) {
      return es;
    }, identity8));
  };
  var raise = function(o) {
    return liftF(new Raise(o, unit));
  };
  var query = function() {
    return function(dictIsSymbol) {
      var lookup13 = lookup4(dictIsSymbol);
      return function(dictOrd) {
        var lookup23 = lookup13(dictOrd);
        return function(label5) {
          return function(p4) {
            return function(q2) {
              return liftF(new ChildQuery2(mkChildQueryBox(new ChildQuery(function(dictApplicative) {
                var pure110 = pure(dictApplicative);
                return function(k) {
                  var $177 = maybe(pure110(Nothing.value))(k);
                  var $178 = lookup23(label5)(p4);
                  return function($179) {
                    return $177($178($179));
                  };
                };
              }, q2, identity8))));
            };
          };
        };
      };
    };
  };
  var ordSubscriptionId = ordInt;
  var ordForkId = ordInt;
  var monoidHalogenM = function(dictMonoid) {
    return monoidFree(dictMonoid);
  };
  var monadHalogenM = freeMonad;
  var monadStateHalogenM = {
    state: function($181) {
      return HalogenM(liftF(State.create($181)));
    },
    Monad0: function() {
      return monadHalogenM;
    }
  };
  var monadEffectHalogenM = function(dictMonadEffect) {
    return {
      liftEffect: function() {
        var $186 = liftEffect(dictMonadEffect);
        return function($187) {
          return HalogenM(liftF(Lift2.create($186($187))));
        };
      }(),
      Monad0: function() {
        return monadHalogenM;
      }
    };
  };
  var monadAffHalogenM = function(dictMonadAff) {
    var monadEffectHalogenM1 = monadEffectHalogenM(dictMonadAff.MonadEffect0());
    return {
      liftAff: function() {
        var $188 = liftAff(dictMonadAff);
        return function($189) {
          return HalogenM(liftF(Lift2.create($188($189))));
        };
      }(),
      MonadEffect0: function() {
        return monadEffectHalogenM1;
      }
    };
  };
  var functorHalogenM = freeFunctor;
  var bindHalogenM = freeBind;
  var bind4 = /* @__PURE__ */ bind(bindHalogenM);
  var applicativeHalogenM = freeApplicative;
  var pure5 = /* @__PURE__ */ pure(applicativeHalogenM);
  var monadRecHalogenM = {
    tailRecM: function(k) {
      return function(a2) {
        return bind4(k(a2))(function(v) {
          if (v instanceof Loop) {
            return tailRecM(monadRecHalogenM)(k)(v.value0);
          }
          ;
          if (v instanceof Done) {
            return pure5(v.value0);
          }
          ;
          throw new Error("Failed pattern match at Halogen.Query.HalogenM (line 105, column 26 - line 107, column 21): " + [v.constructor.name]);
        });
      };
    },
    Monad0: function() {
      return monadHalogenM;
    }
  };

  // output/Halogen.Query.HalogenQ/index.js
  var Initialize = /* @__PURE__ */ function() {
    function Initialize5(value0) {
      this.value0 = value0;
    }
    ;
    Initialize5.create = function(value0) {
      return new Initialize5(value0);
    };
    return Initialize5;
  }();
  var Finalize = /* @__PURE__ */ function() {
    function Finalize5(value0) {
      this.value0 = value0;
    }
    ;
    Finalize5.create = function(value0) {
      return new Finalize5(value0);
    };
    return Finalize5;
  }();
  var Receive = /* @__PURE__ */ function() {
    function Receive2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Receive2.create = function(value0) {
      return function(value1) {
        return new Receive2(value0, value1);
      };
    };
    return Receive2;
  }();
  var Action2 = /* @__PURE__ */ function() {
    function Action3(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Action3.create = function(value0) {
      return function(value1) {
        return new Action3(value0, value1);
      };
    };
    return Action3;
  }();
  var Query = /* @__PURE__ */ function() {
    function Query2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Query2.create = function(value0) {
      return function(value1) {
        return new Query2(value0, value1);
      };
    };
    return Query2;
  }();

  // output/Halogen.VDom.Thunk/index.js
  var $runtime_lazy6 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var unsafeEqThunk = function(v, v1) {
    return refEq2(v.value0, v1.value0) && (refEq2(v.value1, v1.value1) && v.value1(v.value3, v1.value3));
  };
  var runThunk = function(v) {
    return v.value2(v.value3);
  };
  var buildThunk = function(toVDom) {
    var haltThunk = function(state3) {
      return halt(state3.vdom);
    };
    var $lazy_patchThunk = $runtime_lazy6("patchThunk", "Halogen.VDom.Thunk", function() {
      return function(state3, t2) {
        var $48 = unsafeEqThunk(state3.thunk, t2);
        if ($48) {
          return mkStep(new Step(extract2(state3.vdom), state3, $lazy_patchThunk(112), haltThunk));
        }
        ;
        var vdom = step3(state3.vdom, toVDom(runThunk(t2)));
        return mkStep(new Step(extract2(vdom), {
          vdom,
          thunk: t2
        }, $lazy_patchThunk(115), haltThunk));
      };
    });
    var patchThunk = $lazy_patchThunk(108);
    var renderThunk = function(spec) {
      return function(t) {
        var vdom = buildVDom(spec)(toVDom(runThunk(t)));
        return mkStep(new Step(extract2(vdom), {
          thunk: t,
          vdom
        }, patchThunk, haltThunk));
      };
    };
    return renderThunk;
  };

  // output/Halogen.Component/index.js
  var voidLeft2 = /* @__PURE__ */ voidLeft(functorHalogenM);
  var traverse_3 = /* @__PURE__ */ traverse_(applicativeHalogenM)(foldableMaybe);
  var map10 = /* @__PURE__ */ map(functorHalogenM);
  var pure6 = /* @__PURE__ */ pure(applicativeHalogenM);
  var lookup5 = /* @__PURE__ */ lookup2();
  var pop3 = /* @__PURE__ */ pop2();
  var insert3 = /* @__PURE__ */ insert2();
  var ComponentSlot = /* @__PURE__ */ function() {
    function ComponentSlot2(value0) {
      this.value0 = value0;
    }
    ;
    ComponentSlot2.create = function(value0) {
      return new ComponentSlot2(value0);
    };
    return ComponentSlot2;
  }();
  var ThunkSlot = /* @__PURE__ */ function() {
    function ThunkSlot2(value0) {
      this.value0 = value0;
    }
    ;
    ThunkSlot2.create = function(value0) {
      return new ThunkSlot2(value0);
    };
    return ThunkSlot2;
  }();
  var unComponentSlot = unsafeCoerce2;
  var unComponent = unsafeCoerce2;
  var mkEval = function(args) {
    return function(v) {
      if (v instanceof Initialize) {
        return voidLeft2(traverse_3(args.handleAction)(args.initialize))(v.value0);
      }
      ;
      if (v instanceof Finalize) {
        return voidLeft2(traverse_3(args.handleAction)(args.finalize))(v.value0);
      }
      ;
      if (v instanceof Receive) {
        return voidLeft2(traverse_3(args.handleAction)(args.receive(v.value0)))(v.value1);
      }
      ;
      if (v instanceof Action2) {
        return voidLeft2(args.handleAction(v.value0))(v.value1);
      }
      ;
      if (v instanceof Query) {
        return unCoyoneda(function(g) {
          var $45 = map10(maybe(v.value1(unit))(g));
          return function($46) {
            return $45(args.handleQuery($46));
          };
        })(v.value0);
      }
      ;
      throw new Error("Failed pattern match at Halogen.Component (line 182, column 15 - line 192, column 71): " + [v.constructor.name]);
    };
  };
  var mkComponentSlot = unsafeCoerce2;
  var mkComponent = unsafeCoerce2;
  var defaultEval = /* @__PURE__ */ function() {
    return {
      handleAction: $$const(pure6(unit)),
      handleQuery: $$const(pure6(Nothing.value)),
      receive: $$const(Nothing.value),
      initialize: Nothing.value,
      finalize: Nothing.value
    };
  }();
  var componentSlot = function() {
    return function(dictIsSymbol) {
      var lookup13 = lookup5(dictIsSymbol);
      var pop12 = pop3(dictIsSymbol);
      var insert13 = insert3(dictIsSymbol);
      return function(dictOrd) {
        var lookup23 = lookup13(dictOrd);
        var pop22 = pop12(dictOrd);
        var insert22 = insert13(dictOrd);
        return function(label5) {
          return function(p4) {
            return function(comp) {
              return function(input3) {
                return function(output2) {
                  return mkComponentSlot({
                    get: lookup23(label5)(p4),
                    pop: pop22(label5)(p4),
                    set: insert22(label5)(p4),
                    component: comp,
                    input: input3,
                    output: output2
                  });
                };
              };
            };
          };
        };
      };
    };
  };

  // output/Halogen.HTML.Elements/index.js
  var element2 = /* @__PURE__ */ function() {
    return element(Nothing.value);
  }();
  var h2 = /* @__PURE__ */ element2("h2");
  var h2_ = /* @__PURE__ */ h2([]);
  var h5 = /* @__PURE__ */ element2("h5");
  var h5_ = /* @__PURE__ */ h5([]);
  var i = /* @__PURE__ */ element2("i");
  var input2 = function(props) {
    return element2("input")(props)([]);
  };
  var label4 = /* @__PURE__ */ element2("label");
  var li = /* @__PURE__ */ element2("li");
  var li_ = /* @__PURE__ */ li([]);
  var main = /* @__PURE__ */ element2("main");
  var nav = /* @__PURE__ */ element2("nav");
  var option = /* @__PURE__ */ element2("option");
  var p = /* @__PURE__ */ element2("p");
  var p_ = /* @__PURE__ */ p([]);
  var pre = /* @__PURE__ */ element2("pre");
  var pre_ = /* @__PURE__ */ pre([]);
  var select3 = /* @__PURE__ */ element2("select");
  var small = /* @__PURE__ */ element2("small");
  var small_ = /* @__PURE__ */ small([]);
  var span3 = /* @__PURE__ */ element2("span");
  var strong = /* @__PURE__ */ element2("strong");
  var strong_ = /* @__PURE__ */ strong([]);
  var table = /* @__PURE__ */ element2("table");
  var table_ = /* @__PURE__ */ table([]);
  var tbody = /* @__PURE__ */ element2("tbody");
  var tbody_ = /* @__PURE__ */ tbody([]);
  var td = /* @__PURE__ */ element2("td");
  var thead = /* @__PURE__ */ element2("thead");
  var thead_ = /* @__PURE__ */ thead([]);
  var tr = /* @__PURE__ */ element2("tr");
  var tr_ = /* @__PURE__ */ tr([]);
  var ul = /* @__PURE__ */ element2("ul");
  var ul_ = /* @__PURE__ */ ul([]);
  var div2 = /* @__PURE__ */ element2("div");
  var div_ = /* @__PURE__ */ div2([]);
  var button = /* @__PURE__ */ element2("button");
  var br = function(props) {
    return element2("br")(props)([]);
  };
  var br_ = /* @__PURE__ */ br([]);
  var a = /* @__PURE__ */ element2("a");

  // output/Halogen.HTML.Properties/index.js
  var unwrap2 = /* @__PURE__ */ unwrap();
  var prop2 = function(dictIsProp) {
    return prop(dictIsProp);
  };
  var prop1 = /* @__PURE__ */ prop2(isPropBoolean);
  var prop22 = /* @__PURE__ */ prop2(isPropString);
  var selected2 = /* @__PURE__ */ prop1("selected");
  var target5 = /* @__PURE__ */ prop22("target");
  var type_17 = function(dictIsProp) {
    return prop2(dictIsProp)("type");
  };
  var value12 = function(dictIsProp) {
    return prop2(dictIsProp)("value");
  };
  var placeholder3 = /* @__PURE__ */ prop22("placeholder");
  var id2 = /* @__PURE__ */ prop22("id");
  var href4 = /* @__PURE__ */ prop22("href");
  var download3 = /* @__PURE__ */ prop22("download");
  var classes = /* @__PURE__ */ function() {
    var $32 = prop22("className");
    var $33 = joinWith(" ");
    var $34 = map(functorArray)(unwrap2);
    return function($35) {
      return $32($33($34($35)));
    };
  }();
  var class_ = /* @__PURE__ */ function() {
    var $36 = prop22("className");
    return function($37) {
      return $36(unwrap2($37));
    };
  }();
  var attr2 = /* @__PURE__ */ function() {
    return attr(Nothing.value);
  }();
  var style = /* @__PURE__ */ attr2("style");

  // output/Halogen.HTML/index.js
  var componentSlot2 = /* @__PURE__ */ componentSlot();
  var slot = function() {
    return function(dictIsSymbol) {
      var componentSlot1 = componentSlot2(dictIsSymbol);
      return function(dictOrd) {
        var componentSlot22 = componentSlot1(dictOrd);
        return function(label5) {
          return function(p4) {
            return function(component5) {
              return function(input3) {
                return function(outputQuery) {
                  return widget(new ComponentSlot(componentSlot22(label5)(p4)(component5)(input3)(function($11) {
                    return Just.create(outputQuery($11));
                  })));
                };
              };
            };
          };
        };
      };
    };
  };

  // output/Halogen.Query/index.js
  var $$void5 = /* @__PURE__ */ $$void(functorHalogenM);
  var query2 = /* @__PURE__ */ query();
  var identity9 = /* @__PURE__ */ identity(categoryFn);
  var tell2 = function() {
    return function(dictIsSymbol) {
      var query1 = query2(dictIsSymbol);
      return function(dictOrd) {
        var query22 = query1(dictOrd);
        return function(slot3) {
          return function(label5) {
            return function(req) {
              return $$void5(query22(slot3)(label5)(req(unit)));
            };
          };
        };
      };
    };
  };
  var request = function() {
    return function(dictIsSymbol) {
      var query1 = query2(dictIsSymbol);
      return function(dictOrd) {
        var query22 = query1(dictOrd);
        return function(slot3) {
          return function(label5) {
            return function(req) {
              return query22(slot3)(label5)(req(identity9));
            };
          };
        };
      };
    };
  };

  // output/Halogen.Aff.Driver.State/index.js
  var unRenderStateX = unsafeCoerce2;
  var unDriverStateX = unsafeCoerce2;
  var renderStateX_ = function(dictApplicative) {
    var traverse_8 = traverse_(dictApplicative)(foldableMaybe);
    return function(f) {
      return unDriverStateX(function(st) {
        return traverse_8(f)(st.rendering);
      });
    };
  };
  var mkRenderStateX = unsafeCoerce2;
  var renderStateX = function(dictFunctor) {
    return function(f) {
      return unDriverStateX(function(st) {
        return mkRenderStateX(f(st.rendering));
      });
    };
  };
  var mkDriverStateXRef = unsafeCoerce2;
  var mapDriverState = function(f) {
    return function(v) {
      return f(v);
    };
  };
  var initDriverState = function(component5) {
    return function(input3) {
      return function(handler3) {
        return function(lchs) {
          return function __do2() {
            var selfRef = $$new({})();
            var childrenIn = $$new(empty3)();
            var childrenOut = $$new(empty3)();
            var handlerRef = $$new(handler3)();
            var pendingQueries = $$new(new Just(Nil.value))();
            var pendingOuts = $$new(new Just(Nil.value))();
            var pendingHandlers = $$new(Nothing.value)();
            var fresh2 = $$new(1)();
            var subscriptions = $$new(new Just(empty2))();
            var forks = $$new(empty2)();
            var ds = {
              component: component5,
              state: component5.initialState(input3),
              refs: empty2,
              children: empty3,
              childrenIn,
              childrenOut,
              selfRef,
              handlerRef,
              pendingQueries,
              pendingOuts,
              pendingHandlers,
              rendering: Nothing.value,
              fresh: fresh2,
              subscriptions,
              forks,
              lifecycleHandlers: lchs
            };
            write(ds)(selfRef)();
            return mkDriverStateXRef(selfRef);
          };
        };
      };
    };
  };

  // output/Halogen.Aff.Driver.Eval/index.js
  var traverse_4 = /* @__PURE__ */ traverse_(applicativeEffect)(foldableMaybe);
  var bindFlipped5 = /* @__PURE__ */ bindFlipped(bindMaybe);
  var lookup6 = /* @__PURE__ */ lookup(ordSubscriptionId);
  var bind12 = /* @__PURE__ */ bind(bindAff);
  var liftEffect4 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var discard3 = /* @__PURE__ */ discard(discardUnit);
  var discard1 = /* @__PURE__ */ discard3(bindAff);
  var traverse_12 = /* @__PURE__ */ traverse_(applicativeAff);
  var traverse_22 = /* @__PURE__ */ traverse_12(foldableList);
  var fork3 = /* @__PURE__ */ fork(monadForkAff);
  var parSequence_3 = /* @__PURE__ */ parSequence_(parallelAff)(foldableList);
  var pure7 = /* @__PURE__ */ pure(applicativeAff);
  var map13 = /* @__PURE__ */ map(functorCoyoneda);
  var parallel2 = /* @__PURE__ */ parallel(parallelAff);
  var map14 = /* @__PURE__ */ map(functorAff);
  var sequential2 = /* @__PURE__ */ sequential(parallelAff);
  var map22 = /* @__PURE__ */ map(functorMaybe);
  var insert4 = /* @__PURE__ */ insert(ordSubscriptionId);
  var retractFreeAp2 = /* @__PURE__ */ retractFreeAp(applicativeParAff);
  var $$delete2 = /* @__PURE__ */ $$delete(ordForkId);
  var unlessM2 = /* @__PURE__ */ unlessM(monadEffect);
  var insert12 = /* @__PURE__ */ insert(ordForkId);
  var traverse_32 = /* @__PURE__ */ traverse_12(foldableMaybe);
  var lookup12 = /* @__PURE__ */ lookup(ordForkId);
  var lookup22 = /* @__PURE__ */ lookup(ordString);
  var foldFree2 = /* @__PURE__ */ foldFree(monadRecAff);
  var alter2 = /* @__PURE__ */ alter(ordString);
  var unsubscribe3 = function(sid) {
    return function(ref2) {
      return function __do2() {
        var v = read(ref2)();
        var subs = read(v.subscriptions)();
        return traverse_4(unsubscribe)(bindFlipped5(lookup6(sid))(subs))();
      };
    };
  };
  var queueOrRun = function(ref2) {
    return function(au) {
      return bind12(liftEffect4(read(ref2)))(function(v) {
        if (v instanceof Nothing) {
          return au;
        }
        ;
        if (v instanceof Just) {
          return liftEffect4(write(new Just(new Cons(au, v.value0)))(ref2));
        }
        ;
        throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 188, column 33 - line 190, column 57): " + [v.constructor.name]);
      });
    };
  };
  var handleLifecycle = function(lchs) {
    return function(f) {
      return discard1(liftEffect4(write({
        initializers: Nil.value,
        finalizers: Nil.value
      })(lchs)))(function() {
        return bind12(liftEffect4(f))(function(result) {
          return bind12(liftEffect4(read(lchs)))(function(v) {
            return discard1(traverse_22(fork3)(v.finalizers))(function() {
              return discard1(parSequence_3(v.initializers))(function() {
                return pure7(result);
              });
            });
          });
        });
      });
    };
  };
  var handleAff = /* @__PURE__ */ runAff_(/* @__PURE__ */ either(throwException)(/* @__PURE__ */ $$const(/* @__PURE__ */ pure(applicativeEffect)(unit))));
  var fresh = function(f) {
    return function(ref2) {
      return bind12(liftEffect4(read(ref2)))(function(v) {
        return liftEffect4(modify$prime(function(i2) {
          return {
            state: i2 + 1 | 0,
            value: f(i2)
          };
        })(v.fresh));
      });
    };
  };
  var evalQ = function(render4) {
    return function(ref2) {
      return function(q2) {
        return bind12(liftEffect4(read(ref2)))(function(v) {
          return evalM(render4)(ref2)(v["component"]["eval"](new Query(map13(Just.create)(liftCoyoneda(q2)), $$const(Nothing.value))));
        });
      };
    };
  };
  var evalM = function(render4) {
    return function(initRef) {
      return function(v) {
        var evalChildQuery = function(ref2) {
          return function(cqb) {
            return bind12(liftEffect4(read(ref2)))(function(v1) {
              return unChildQueryBox(function(v2) {
                var evalChild = function(v3) {
                  return parallel2(bind12(liftEffect4(read(v3)))(function(dsx) {
                    return unDriverStateX(function(ds) {
                      return evalQ(render4)(ds.selfRef)(v2.value1);
                    })(dsx);
                  }));
                };
                return map14(v2.value2)(sequential2(v2.value0(applicativeParAff)(evalChild)(v1.children)));
              })(cqb);
            });
          };
        };
        var go2 = function(ref2) {
          return function(v1) {
            if (v1 instanceof State) {
              return bind12(liftEffect4(read(ref2)))(function(v2) {
                var v3 = v1.value0(v2.state);
                if (unsafeRefEq(v2.state)(v3.value1)) {
                  return pure7(v3.value0);
                }
                ;
                if (otherwise) {
                  return discard1(liftEffect4(write({
                    component: v2.component,
                    state: v3.value1,
                    refs: v2.refs,
                    children: v2.children,
                    childrenIn: v2.childrenIn,
                    childrenOut: v2.childrenOut,
                    selfRef: v2.selfRef,
                    handlerRef: v2.handlerRef,
                    pendingQueries: v2.pendingQueries,
                    pendingOuts: v2.pendingOuts,
                    pendingHandlers: v2.pendingHandlers,
                    rendering: v2.rendering,
                    fresh: v2.fresh,
                    subscriptions: v2.subscriptions,
                    forks: v2.forks,
                    lifecycleHandlers: v2.lifecycleHandlers
                  })(ref2)))(function() {
                    return discard1(handleLifecycle(v2.lifecycleHandlers)(render4(v2.lifecycleHandlers)(ref2)))(function() {
                      return pure7(v3.value0);
                    });
                  });
                }
                ;
                throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 86, column 7 - line 92, column 21): " + [v3.constructor.name]);
              });
            }
            ;
            if (v1 instanceof Subscribe) {
              return bind12(fresh(SubscriptionId)(ref2))(function(sid) {
                return bind12(liftEffect4(subscribe(v1.value0(sid))(function(act) {
                  return handleAff(evalF(render4)(ref2)(new Action(act)));
                })))(function(finalize) {
                  return bind12(liftEffect4(read(ref2)))(function(v2) {
                    return discard1(liftEffect4(modify_(map22(insert4(sid)(finalize)))(v2.subscriptions)))(function() {
                      return pure7(v1.value1(sid));
                    });
                  });
                });
              });
            }
            ;
            if (v1 instanceof Unsubscribe) {
              return discard1(liftEffect4(unsubscribe3(v1.value0)(ref2)))(function() {
                return pure7(v1.value1);
              });
            }
            ;
            if (v1 instanceof Lift2) {
              return v1.value0;
            }
            ;
            if (v1 instanceof ChildQuery2) {
              return evalChildQuery(ref2)(v1.value0);
            }
            ;
            if (v1 instanceof Raise) {
              return bind12(liftEffect4(read(ref2)))(function(v2) {
                return bind12(liftEffect4(read(v2.handlerRef)))(function(handler3) {
                  return discard1(queueOrRun(v2.pendingOuts)(handler3(v1.value0)))(function() {
                    return pure7(v1.value1);
                  });
                });
              });
            }
            ;
            if (v1 instanceof Par) {
              return sequential2(retractFreeAp2(hoistFreeAp(function() {
                var $118 = evalM(render4)(ref2);
                return function($119) {
                  return parallel2($118($119));
                };
              }())(v1.value0)));
            }
            ;
            if (v1 instanceof Fork) {
              return bind12(fresh(ForkId)(ref2))(function(fid) {
                return bind12(liftEffect4(read(ref2)))(function(v2) {
                  return bind12(liftEffect4($$new(false)))(function(doneRef) {
                    return bind12(fork3($$finally(liftEffect4(function __do2() {
                      modify_($$delete2(fid))(v2.forks)();
                      return write(true)(doneRef)();
                    }))(evalM(render4)(ref2)(v1.value0))))(function(fiber) {
                      return discard1(liftEffect4(unlessM2(read(doneRef))(modify_(insert12(fid)(fiber))(v2.forks))))(function() {
                        return pure7(v1.value1(fid));
                      });
                    });
                  });
                });
              });
            }
            ;
            if (v1 instanceof Join) {
              return bind12(liftEffect4(read(ref2)))(function(v2) {
                return bind12(liftEffect4(read(v2.forks)))(function(forkMap) {
                  return discard1(traverse_32(joinFiber)(lookup12(v1.value0)(forkMap)))(function() {
                    return pure7(v1.value1);
                  });
                });
              });
            }
            ;
            if (v1 instanceof Kill) {
              return bind12(liftEffect4(read(ref2)))(function(v2) {
                return bind12(liftEffect4(read(v2.forks)))(function(forkMap) {
                  return discard1(traverse_32(killFiber(error("Cancelled")))(lookup12(v1.value0)(forkMap)))(function() {
                    return pure7(v1.value1);
                  });
                });
              });
            }
            ;
            if (v1 instanceof GetRef) {
              return bind12(liftEffect4(read(ref2)))(function(v2) {
                return pure7(v1.value1(lookup22(v1.value0)(v2.refs)));
              });
            }
            ;
            throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 83, column 12 - line 139, column 33): " + [v1.constructor.name]);
          };
        };
        return foldFree2(go2(initRef))(v);
      };
    };
  };
  var evalF = function(render4) {
    return function(ref2) {
      return function(v) {
        if (v instanceof RefUpdate) {
          return liftEffect4(flip(modify_)(ref2)(mapDriverState(function(st) {
            return {
              component: st.component,
              state: st.state,
              refs: alter2($$const(v.value1))(v.value0)(st.refs),
              children: st.children,
              childrenIn: st.childrenIn,
              childrenOut: st.childrenOut,
              selfRef: st.selfRef,
              handlerRef: st.handlerRef,
              pendingQueries: st.pendingQueries,
              pendingOuts: st.pendingOuts,
              pendingHandlers: st.pendingHandlers,
              rendering: st.rendering,
              fresh: st.fresh,
              subscriptions: st.subscriptions,
              forks: st.forks,
              lifecycleHandlers: st.lifecycleHandlers
            };
          })));
        }
        ;
        if (v instanceof Action) {
          return bind12(liftEffect4(read(ref2)))(function(v1) {
            return evalM(render4)(ref2)(v1["component"]["eval"](new Action2(v.value0, unit)));
          });
        }
        ;
        throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 52, column 20 - line 58, column 62): " + [v.constructor.name]);
      };
    };
  };

  // output/Halogen.Aff.Driver/index.js
  var bind5 = /* @__PURE__ */ bind(bindEffect);
  var discard4 = /* @__PURE__ */ discard(discardUnit);
  var for_2 = /* @__PURE__ */ for_(applicativeEffect)(foldableMaybe);
  var traverse_5 = /* @__PURE__ */ traverse_(applicativeAff)(foldableList);
  var fork4 = /* @__PURE__ */ fork(monadForkAff);
  var bindFlipped6 = /* @__PURE__ */ bindFlipped(bindEffect);
  var traverse_13 = /* @__PURE__ */ traverse_(applicativeEffect);
  var traverse_23 = /* @__PURE__ */ traverse_13(foldableMaybe);
  var traverse_33 = /* @__PURE__ */ traverse_13(foldableMap);
  var discard22 = /* @__PURE__ */ discard4(bindAff);
  var parSequence_4 = /* @__PURE__ */ parSequence_(parallelAff)(foldableList);
  var liftEffect5 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var pure8 = /* @__PURE__ */ pure(applicativeEffect);
  var map15 = /* @__PURE__ */ map(functorEffect);
  var pure12 = /* @__PURE__ */ pure(applicativeAff);
  var when2 = /* @__PURE__ */ when(applicativeEffect);
  var renderStateX2 = /* @__PURE__ */ renderStateX(functorEffect);
  var $$void6 = /* @__PURE__ */ $$void(functorAff);
  var foreachSlot2 = /* @__PURE__ */ foreachSlot(applicativeEffect);
  var renderStateX_2 = /* @__PURE__ */ renderStateX_(applicativeEffect);
  var tailRecM3 = /* @__PURE__ */ tailRecM(monadRecEffect);
  var voidLeft3 = /* @__PURE__ */ voidLeft(functorEffect);
  var bind13 = /* @__PURE__ */ bind(bindAff);
  var liftEffect1 = /* @__PURE__ */ liftEffect(monadEffectEffect);
  var newLifecycleHandlers = /* @__PURE__ */ function() {
    return $$new({
      initializers: Nil.value,
      finalizers: Nil.value
    });
  }();
  var handlePending = function(ref2) {
    return function __do2() {
      var queue = read(ref2)();
      write(Nothing.value)(ref2)();
      return for_2(queue)(function() {
        var $58 = traverse_5(fork4);
        return function($59) {
          return handleAff($58(reverse($59)));
        };
      }())();
    };
  };
  var cleanupSubscriptionsAndForks = function(v) {
    return function __do2() {
      bindFlipped6(traverse_23(traverse_33(unsubscribe)))(read(v.subscriptions))();
      write(Nothing.value)(v.subscriptions)();
      bindFlipped6(traverse_33(function() {
        var $60 = killFiber(error("finalized"));
        return function($61) {
          return handleAff($60($61));
        };
      }()))(read(v.forks))();
      return write(empty2)(v.forks)();
    };
  };
  var runUI = function(renderSpec2) {
    return function(component5) {
      return function(i2) {
        var squashChildInitializers = function(lchs) {
          return function(preInits) {
            return unDriverStateX(function(st) {
              var parentInitializer = evalM(render4)(st.selfRef)(st["component"]["eval"](new Initialize(unit)));
              return modify_(function(handlers) {
                return {
                  initializers: new Cons(discard22(parSequence_4(reverse(handlers.initializers)))(function() {
                    return discard22(parentInitializer)(function() {
                      return liftEffect5(function __do2() {
                        handlePending(st.pendingQueries)();
                        return handlePending(st.pendingOuts)();
                      });
                    });
                  }), preInits),
                  finalizers: handlers.finalizers
                };
              })(lchs);
            });
          };
        };
        var runComponent = function(lchs) {
          return function(handler3) {
            return function(j) {
              return unComponent(function(c) {
                return function __do2() {
                  var lchs$prime = newLifecycleHandlers();
                  var $$var2 = initDriverState(c)(j)(handler3)(lchs$prime)();
                  var pre2 = read(lchs)();
                  write({
                    initializers: Nil.value,
                    finalizers: pre2.finalizers
                  })(lchs)();
                  bindFlipped6(unDriverStateX(function() {
                    var $62 = render4(lchs);
                    return function($63) {
                      return $62(function(v) {
                        return v.selfRef;
                      }($63));
                    };
                  }()))(read($$var2))();
                  bindFlipped6(squashChildInitializers(lchs)(pre2.initializers))(read($$var2))();
                  return $$var2;
                };
              });
            };
          };
        };
        var renderChild = function(lchs) {
          return function(handler3) {
            return function(childrenInRef) {
              return function(childrenOutRef) {
                return unComponentSlot(function(slot3) {
                  return function __do2() {
                    var childrenIn = map15(slot3.pop)(read(childrenInRef))();
                    var $$var2 = function() {
                      if (childrenIn instanceof Just) {
                        write(childrenIn.value0.value1)(childrenInRef)();
                        var dsx = read(childrenIn.value0.value0)();
                        unDriverStateX(function(st) {
                          return function __do3() {
                            flip(write)(st.handlerRef)(function() {
                              var $64 = maybe(pure12(unit))(handler3);
                              return function($65) {
                                return $64(slot3.output($65));
                              };
                            }())();
                            return handleAff(evalM(render4)(st.selfRef)(st["component"]["eval"](new Receive(slot3.input, unit))))();
                          };
                        })(dsx)();
                        return childrenIn.value0.value0;
                      }
                      ;
                      if (childrenIn instanceof Nothing) {
                        return runComponent(lchs)(function() {
                          var $66 = maybe(pure12(unit))(handler3);
                          return function($67) {
                            return $66(slot3.output($67));
                          };
                        }())(slot3.input)(slot3.component)();
                      }
                      ;
                      throw new Error("Failed pattern match at Halogen.Aff.Driver (line 213, column 14 - line 222, column 98): " + [childrenIn.constructor.name]);
                    }();
                    var isDuplicate = map15(function($68) {
                      return isJust(slot3.get($68));
                    })(read(childrenOutRef))();
                    when2(isDuplicate)(warn("Halogen: Duplicate slot address was detected during rendering, unexpected results may occur"))();
                    modify_(slot3.set($$var2))(childrenOutRef)();
                    return bind5(read($$var2))(renderStateX2(function(v) {
                      if (v instanceof Nothing) {
                        return $$throw("Halogen internal error: child was not initialized in renderChild");
                      }
                      ;
                      if (v instanceof Just) {
                        return pure8(renderSpec2.renderChild(v.value0));
                      }
                      ;
                      throw new Error("Failed pattern match at Halogen.Aff.Driver (line 227, column 37 - line 229, column 50): " + [v.constructor.name]);
                    }))();
                  };
                });
              };
            };
          };
        };
        var render4 = function(lchs) {
          return function($$var2) {
            return function __do2() {
              var v = read($$var2)();
              var shouldProcessHandlers = map15(isNothing)(read(v.pendingHandlers))();
              when2(shouldProcessHandlers)(write(new Just(Nil.value))(v.pendingHandlers))();
              write(empty3)(v.childrenOut)();
              write(v.children)(v.childrenIn)();
              var handler3 = function() {
                var $69 = queueOrRun(v.pendingHandlers);
                var $70 = evalF(render4)(v.selfRef);
                return function($71) {
                  return $69($$void6($70($71)));
                };
              }();
              var childHandler = function() {
                var $72 = queueOrRun(v.pendingQueries);
                return function($73) {
                  return $72(handler3(Action.create($73)));
                };
              }();
              var rendering = renderSpec2.render(function($74) {
                return handleAff(handler3($74));
              })(renderChild(lchs)(childHandler)(v.childrenIn)(v.childrenOut))(v.component.render(v.state))(v.rendering)();
              var children2 = read(v.childrenOut)();
              var childrenIn = read(v.childrenIn)();
              foreachSlot2(childrenIn)(function(v1) {
                return function __do3() {
                  var childDS = read(v1)();
                  renderStateX_2(renderSpec2.removeChild)(childDS)();
                  return finalize(lchs)(childDS)();
                };
              })();
              flip(modify_)(v.selfRef)(mapDriverState(function(ds$prime) {
                return {
                  component: ds$prime.component,
                  state: ds$prime.state,
                  refs: ds$prime.refs,
                  children: children2,
                  childrenIn: ds$prime.childrenIn,
                  childrenOut: ds$prime.childrenOut,
                  selfRef: ds$prime.selfRef,
                  handlerRef: ds$prime.handlerRef,
                  pendingQueries: ds$prime.pendingQueries,
                  pendingOuts: ds$prime.pendingOuts,
                  pendingHandlers: ds$prime.pendingHandlers,
                  rendering: new Just(rendering),
                  fresh: ds$prime.fresh,
                  subscriptions: ds$prime.subscriptions,
                  forks: ds$prime.forks,
                  lifecycleHandlers: ds$prime.lifecycleHandlers
                };
              }))();
              return when2(shouldProcessHandlers)(flip(tailRecM3)(unit)(function(v1) {
                return function __do3() {
                  var handlers = read(v.pendingHandlers)();
                  write(new Just(Nil.value))(v.pendingHandlers)();
                  traverse_23(function() {
                    var $75 = traverse_5(fork4);
                    return function($76) {
                      return handleAff($75(reverse($76)));
                    };
                  }())(handlers)();
                  var mmore = read(v.pendingHandlers)();
                  var $51 = maybe(false)($$null)(mmore);
                  if ($51) {
                    return voidLeft3(write(Nothing.value)(v.pendingHandlers))(new Done(unit))();
                  }
                  ;
                  return new Loop(unit);
                };
              }))();
            };
          };
        };
        var finalize = function(lchs) {
          return unDriverStateX(function(st) {
            return function __do2() {
              cleanupSubscriptionsAndForks(st)();
              var f = evalM(render4)(st.selfRef)(st["component"]["eval"](new Finalize(unit)));
              modify_(function(handlers) {
                return {
                  initializers: handlers.initializers,
                  finalizers: new Cons(f, handlers.finalizers)
                };
              })(lchs)();
              return foreachSlot2(st.children)(function(v) {
                return function __do3() {
                  var dsx = read(v)();
                  return finalize(lchs)(dsx)();
                };
              })();
            };
          });
        };
        var evalDriver = function(disposed) {
          return function(ref2) {
            return function(q2) {
              return bind13(liftEffect5(read(disposed)))(function(v) {
                if (v) {
                  return pure12(Nothing.value);
                }
                ;
                return evalQ(render4)(ref2)(q2);
              });
            };
          };
        };
        var dispose = function(disposed) {
          return function(lchs) {
            return function(dsx) {
              return handleLifecycle(lchs)(function __do2() {
                var v = read(disposed)();
                if (v) {
                  return unit;
                }
                ;
                write(true)(disposed)();
                finalize(lchs)(dsx)();
                return unDriverStateX(function(v1) {
                  return function __do3() {
                    var v2 = liftEffect1(read(v1.selfRef))();
                    return for_2(v2.rendering)(renderSpec2.dispose)();
                  };
                })(dsx)();
              });
            };
          };
        };
        return bind13(liftEffect5(newLifecycleHandlers))(function(lchs) {
          return bind13(liftEffect5($$new(false)))(function(disposed) {
            return handleLifecycle(lchs)(function __do2() {
              var sio = create3();
              var dsx = bindFlipped6(read)(runComponent(lchs)(function() {
                var $77 = notify(sio.listener);
                return function($78) {
                  return liftEffect5($77($78));
                };
              }())(i2)(component5))();
              return unDriverStateX(function(st) {
                return pure8({
                  query: evalDriver(disposed)(st.selfRef),
                  messages: sio.emitter,
                  dispose: dispose(disposed)(lchs)(dsx)
                });
              })(dsx)();
            });
          });
        });
      };
    };
  };

  // output/Web.DOM.Node/foreign.js
  var getEffProp2 = function(name15) {
    return function(node) {
      return function() {
        return node[name15];
      };
    };
  };
  var baseURI = getEffProp2("baseURI");
  var _ownerDocument = getEffProp2("ownerDocument");
  var _parentNode = getEffProp2("parentNode");
  var _parentElement = getEffProp2("parentElement");
  var childNodes = getEffProp2("childNodes");
  var _firstChild = getEffProp2("firstChild");
  var _lastChild = getEffProp2("lastChild");
  var _previousSibling = getEffProp2("previousSibling");
  var _nextSibling = getEffProp2("nextSibling");
  var _nodeValue = getEffProp2("nodeValue");
  var textContent = getEffProp2("textContent");
  function insertBefore(node1) {
    return function(node2) {
      return function(parent2) {
        return function() {
          parent2.insertBefore(node1, node2);
        };
      };
    };
  }
  function appendChild(node) {
    return function(parent2) {
      return function() {
        parent2.appendChild(node);
      };
    };
  }
  function removeChild2(node) {
    return function(parent2) {
      return function() {
        parent2.removeChild(node);
      };
    };
  }

  // output/Web.DOM.Node/index.js
  var map16 = /* @__PURE__ */ map(functorEffect);
  var parentNode2 = /* @__PURE__ */ function() {
    var $6 = map16(toMaybe);
    return function($7) {
      return $6(_parentNode($7));
    };
  }();
  var nextSibling = /* @__PURE__ */ function() {
    var $15 = map16(toMaybe);
    return function($16) {
      return $15(_nextSibling($16));
    };
  }();

  // output/Halogen.VDom.Driver/index.js
  var $runtime_lazy7 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var $$void7 = /* @__PURE__ */ $$void(functorEffect);
  var pure9 = /* @__PURE__ */ pure(applicativeEffect);
  var traverse_6 = /* @__PURE__ */ traverse_(applicativeEffect)(foldableMaybe);
  var unwrap3 = /* @__PURE__ */ unwrap();
  var when3 = /* @__PURE__ */ when(applicativeEffect);
  var not2 = /* @__PURE__ */ not(/* @__PURE__ */ heytingAlgebraFunction(/* @__PURE__ */ heytingAlgebraFunction(heytingAlgebraBoolean)));
  var identity10 = /* @__PURE__ */ identity(categoryFn);
  var bind14 = /* @__PURE__ */ bind(bindAff);
  var liftEffect6 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var map17 = /* @__PURE__ */ map(functorEffect);
  var bindFlipped7 = /* @__PURE__ */ bindFlipped(bindEffect);
  var substInParent = function(v) {
    return function(v1) {
      return function(v2) {
        if (v1 instanceof Just && v2 instanceof Just) {
          return $$void7(insertBefore(v)(v1.value0)(v2.value0));
        }
        ;
        if (v1 instanceof Nothing && v2 instanceof Just) {
          return $$void7(appendChild(v)(v2.value0));
        }
        ;
        return pure9(unit);
      };
    };
  };
  var removeChild3 = function(v) {
    return function __do2() {
      var npn = parentNode2(v.node)();
      return traverse_6(function(pn) {
        return removeChild2(v.node)(pn);
      })(npn)();
    };
  };
  var mkSpec = function(handler3) {
    return function(renderChildRef) {
      return function(document3) {
        var getNode = unRenderStateX(function(v) {
          return v.node;
        });
        var done = function(st) {
          if (st instanceof Just) {
            return halt(st.value0);
          }
          ;
          return unit;
        };
        var buildWidget2 = function(spec) {
          var buildThunk2 = buildThunk(unwrap3)(spec);
          var $lazy_patch = $runtime_lazy7("patch", "Halogen.VDom.Driver", function() {
            return function(st, slot3) {
              if (st instanceof Just) {
                if (slot3 instanceof ComponentSlot) {
                  halt(st.value0);
                  return $lazy_renderComponentSlot(100)(slot3.value0);
                }
                ;
                if (slot3 instanceof ThunkSlot) {
                  var step$prime = step3(st.value0, slot3.value0);
                  return mkStep(new Step(extract2(step$prime), new Just(step$prime), $lazy_patch(103), done));
                }
                ;
                throw new Error("Failed pattern match at Halogen.VDom.Driver (line 97, column 22 - line 103, column 79): " + [slot3.constructor.name]);
              }
              ;
              return $lazy_render(104)(slot3);
            };
          });
          var $lazy_render = $runtime_lazy7("render", "Halogen.VDom.Driver", function() {
            return function(slot3) {
              if (slot3 instanceof ComponentSlot) {
                return $lazy_renderComponentSlot(86)(slot3.value0);
              }
              ;
              if (slot3 instanceof ThunkSlot) {
                var step4 = buildThunk2(slot3.value0);
                return mkStep(new Step(extract2(step4), new Just(step4), $lazy_patch(89), done));
              }
              ;
              throw new Error("Failed pattern match at Halogen.VDom.Driver (line 84, column 7 - line 89, column 75): " + [slot3.constructor.name]);
            };
          });
          var $lazy_renderComponentSlot = $runtime_lazy7("renderComponentSlot", "Halogen.VDom.Driver", function() {
            return function(cs) {
              var renderChild = read(renderChildRef)();
              var rsx = renderChild(cs)();
              var node = getNode(rsx);
              return mkStep(new Step(node, Nothing.value, $lazy_patch(117), done));
            };
          });
          var patch = $lazy_patch(91);
          var render4 = $lazy_render(82);
          var renderComponentSlot = $lazy_renderComponentSlot(109);
          return render4;
        };
        var buildAttributes = buildProp(handler3);
        return {
          buildWidget: buildWidget2,
          buildAttributes,
          document: document3
        };
      };
    };
  };
  var renderSpec = function(document3) {
    return function(container2) {
      var render4 = function(handler3) {
        return function(child) {
          return function(v) {
            return function(v1) {
              if (v1 instanceof Nothing) {
                return function __do2() {
                  var renderChildRef = $$new(child)();
                  var spec = mkSpec(handler3)(renderChildRef)(document3);
                  var machine = buildVDom(spec)(v);
                  var node = extract2(machine);
                  $$void7(appendChild(node)(toNode(container2)))();
                  return {
                    machine,
                    node,
                    renderChildRef
                  };
                };
              }
              ;
              if (v1 instanceof Just) {
                return function __do2() {
                  write(child)(v1.value0.renderChildRef)();
                  var parent2 = parentNode2(v1.value0.node)();
                  var nextSib = nextSibling(v1.value0.node)();
                  var machine$prime = step3(v1.value0.machine, v);
                  var newNode = extract2(machine$prime);
                  when3(not2(unsafeRefEq)(v1.value0.node)(newNode))(substInParent(newNode)(nextSib)(parent2))();
                  return {
                    machine: machine$prime,
                    node: newNode,
                    renderChildRef: v1.value0.renderChildRef
                  };
                };
              }
              ;
              throw new Error("Failed pattern match at Halogen.VDom.Driver (line 157, column 5 - line 173, column 80): " + [v1.constructor.name]);
            };
          };
        };
      };
      return {
        render: render4,
        renderChild: identity10,
        removeChild: removeChild3,
        dispose: removeChild3
      };
    };
  };
  var runUI2 = function(component5) {
    return function(i2) {
      return function(element3) {
        return bind14(liftEffect6(map17(toDocument)(bindFlipped7(document2)(windowImpl))))(function(document3) {
          return runUI(renderSpec(document3)(element3))(component5)(i2);
        });
      };
    };
  };

  // output/Page/foreign.js
  var bootstrapToastShow = function() {
    var toastElList = [].slice.call(document.querySelectorAll(".toast"));
    var toastList = toastElList.map(function(toastEl) {
      return new bootstrap.Toast(toastEl, { autohide: false });
    });
    toastList.map(function(toast2) {
      toast2.show();
    });
  };

  // output/Data.Show.Generic/foreign.js
  var intercalate2 = function(separator) {
    return function(xs) {
      return xs.join(separator);
    };
  };

  // output/Data.Show.Generic/index.js
  var append6 = /* @__PURE__ */ append(semigroupArray);
  var genericShowArgsNoArguments = {
    genericShowArgs: function(v) {
      return [];
    }
  };
  var genericShowArgs = function(dict) {
    return dict.genericShowArgs;
  };
  var genericShowConstructor = function(dictGenericShowArgs) {
    var genericShowArgs1 = genericShowArgs(dictGenericShowArgs);
    return function(dictIsSymbol) {
      var reflectSymbol2 = reflectSymbol(dictIsSymbol);
      return {
        "genericShow'": function(v) {
          var ctor = reflectSymbol2($$Proxy.value);
          var v1 = genericShowArgs1(v);
          if (v1.length === 0) {
            return ctor;
          }
          ;
          return "(" + (intercalate2(" ")(append6([ctor])(v1)) + ")");
        }
      };
    };
  };
  var genericShow$prime = function(dict) {
    return dict["genericShow'"];
  };
  var genericShowSum = function(dictGenericShow) {
    var genericShow$prime1 = genericShow$prime(dictGenericShow);
    return function(dictGenericShow1) {
      var genericShow$prime2 = genericShow$prime(dictGenericShow1);
      return {
        "genericShow'": function(v) {
          if (v instanceof Inl) {
            return genericShow$prime1(v.value0);
          }
          ;
          if (v instanceof Inr) {
            return genericShow$prime2(v.value0);
          }
          ;
          throw new Error("Failed pattern match at Data.Show.Generic (line 26, column 1 - line 28, column 40): " + [v.constructor.name]);
        }
      };
    };
  };
  var genericShow = function(dictGeneric) {
    var from4 = from(dictGeneric);
    return function(dictGenericShow) {
      var genericShow$prime1 = genericShow$prime(dictGenericShow);
      return function(x) {
        return genericShow$prime1(from4(x));
      };
    };
  };

  // output/Data.UInt/foreign.js
  function from3(val) {
    return val >>> 0;
  }
  function exact(just) {
    return function(nothing) {
      return function(conv) {
        return function(val) {
          var cval = conv(val);
          if (cval == val) {
            return just(cval);
          }
          return nothing;
        };
      };
    };
  }
  function toInt(uval) {
    return uval | 0;
  }
  function toNumber2(uval) {
    return uval;
  }
  function uintEq(x) {
    return function(y) {
      return x == y;
    };
  }
  function toString(x) {
    return x.toString();
  }
  function and2(n1) {
    return function(n2) {
      return (n1 & n2) >>> 0;
    };
  }
  function or2(n1) {
    return function(n2) {
      return (n1 | n2) >>> 0;
    };
  }
  function shl(n1) {
    return function(n2) {
      return n1 << n2 >>> 0;
    };
  }
  function shr(n1) {
    return function(n2) {
      return n1 >> n2 >>> 0;
    };
  }

  // output/Data.UInt/index.js
  var uintShowInstance = {
    show: function(u2) {
      return toString(u2) + "u";
    }
  };
  var uintEqInstance = {
    eq: uintEq
  };
  var toInt$prime = /* @__PURE__ */ function() {
    return exact(Just.create)(Nothing.value)(toInt);
  }();
  var fromInt = from3;
  var fromInt$prime = /* @__PURE__ */ function() {
    return exact(Just.create)(Nothing.value)(fromInt);
  }();

  // output/Parsing/index.js
  var unwrap4 = /* @__PURE__ */ unwrap();
  var ParseState = /* @__PURE__ */ function() {
    function ParseState2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    ParseState2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new ParseState2(value0, value1, value22);
        };
      };
    };
    return ParseState2;
  }();
  var ParseError = /* @__PURE__ */ function() {
    function ParseError2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ParseError2.create = function(value0) {
      return function(value1) {
        return new ParseError2(value0, value1);
      };
    };
    return ParseError2;
  }();
  var More = /* @__PURE__ */ function() {
    function More2(value0) {
      this.value0 = value0;
    }
    ;
    More2.create = function(value0) {
      return new More2(value0);
    };
    return More2;
  }();
  var Lift3 = /* @__PURE__ */ function() {
    function Lift4(value0) {
      this.value0 = value0;
    }
    ;
    Lift4.create = function(value0) {
      return new Lift4(value0);
    };
    return Lift4;
  }();
  var Stop = /* @__PURE__ */ function() {
    function Stop2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Stop2.create = function(value0) {
      return function(value1) {
        return new Stop2(value0, value1);
      };
    };
    return Stop2;
  }();
  var monadTransParserT = {
    lift: function(dictMonad) {
      var map31 = map(dictMonad.Bind1().Apply0().Functor0());
      return function(m) {
        return function(state1, v, lift$prime, v1, done) {
          return lift$prime(map31(function(a2) {
            return function(v2) {
              return done(state1, a2);
            };
          })(m));
        };
      };
    }
  };
  var lazyParserT = {
    defer: function(f) {
      var m = defer2(f);
      return function(state1, more, lift1, $$throw2, done) {
        var v = force(m);
        return v(state1, more, lift1, $$throw2, done);
      };
    }
  };
  var functorParserT = {
    map: function(f) {
      return function(v) {
        return function(state1, more, lift1, $$throw2, done) {
          return more(function(v1) {
            return v(state1, more, lift1, $$throw2, function(state22, a2) {
              return more(function(v2) {
                return done(state22, f(a2));
              });
            });
          });
        };
      };
    }
  };
  var applyParserT = {
    apply: function(v) {
      return function(v1) {
        return function(state1, more, lift1, $$throw2, done) {
          return more(function(v2) {
            return v(state1, more, lift1, $$throw2, function(state22, f) {
              return more(function(v3) {
                return v1(state22, more, lift1, $$throw2, function(state3, a2) {
                  return more(function(v4) {
                    return done(state3, f(a2));
                  });
                });
              });
            });
          });
        };
      };
    },
    Functor0: function() {
      return functorParserT;
    }
  };
  var bindParserT = {
    bind: function(v) {
      return function(next) {
        return function(state1, more, lift1, $$throw2, done) {
          return more(function(v1) {
            return v(state1, more, lift1, $$throw2, function(state22, a2) {
              return more(function(v2) {
                var v3 = next(a2);
                return v3(state22, more, lift1, $$throw2, done);
              });
            });
          });
        };
      };
    },
    Apply0: function() {
      return applyParserT;
    }
  };
  var bindFlipped8 = /* @__PURE__ */ bindFlipped(bindParserT);
  var applicativeParserT = {
    pure: function(a2) {
      return function(state1, v, v1, v2, done) {
        return done(state1, a2);
      };
    },
    Apply0: function() {
      return applyParserT;
    }
  };
  var pure10 = /* @__PURE__ */ pure(applicativeParserT);
  var monadParserT = {
    Applicative0: function() {
      return applicativeParserT;
    },
    Bind1: function() {
      return bindParserT;
    }
  };
  var monadThrowParseErrorParse = {
    throwError: function(err) {
      return function(state1, v, v1, $$throw2, v2) {
        return $$throw2(state1, err);
      };
    },
    Monad0: function() {
      return monadParserT;
    }
  };
  var throwError3 = /* @__PURE__ */ throwError(monadThrowParseErrorParse);
  var altParserT = {
    alt: function(v) {
      return function(v1) {
        return function(v2, more, lift1, $$throw2, done) {
          return more(function(v3) {
            return v(new ParseState(v2.value0, v2.value1, false), more, lift1, function(v4, err) {
              return more(function(v5) {
                if (v4.value2) {
                  return $$throw2(v4, err);
                }
                ;
                return v1(v2, more, lift1, $$throw2, done);
              });
            }, done);
          });
        };
      };
    },
    Functor0: function() {
      return functorParserT;
    }
  };
  var stateParserT = function(k) {
    return function(state1, v, v1, v2, done) {
      var v3 = k(state1);
      return done(v3.value1, v3.value0);
    };
  };
  var runParserT$prime = function(dictMonadRec) {
    var Monad0 = dictMonadRec.Monad0();
    var map31 = map(Monad0.Bind1().Apply0().Functor0());
    var pure110 = pure(Monad0.Applicative0());
    var tailRecM5 = tailRecM(dictMonadRec);
    return function(state1) {
      return function(v) {
        var go2 = function($copy_step) {
          var $tco_done = false;
          var $tco_result;
          function $tco_loop(step4) {
            var v1 = step4(unit);
            if (v1 instanceof More) {
              $copy_step = v1.value0;
              return;
            }
            ;
            if (v1 instanceof Lift3) {
              $tco_done = true;
              return map31(Loop.create)(v1.value0);
            }
            ;
            if (v1 instanceof Stop) {
              $tco_done = true;
              return pure110(new Done(new Tuple(v1.value1, v1.value0)));
            }
            ;
            throw new Error("Failed pattern match at Parsing (line 152, column 13 - line 158, column 32): " + [v1.constructor.name]);
          }
          ;
          while (!$tco_done) {
            $tco_result = $tco_loop($copy_step);
          }
          ;
          return $tco_result;
        };
        return tailRecM5(go2)(function(v1) {
          return v(state1, More.create, Lift3.create, function(state22, err) {
            return new Stop(state22, new Left(err));
          }, function(state22, res) {
            return new Stop(state22, new Right(res));
          });
        });
      };
    };
  };
  var position2 = /* @__PURE__ */ stateParserT(function(v) {
    return new Tuple(v.value1, v);
  });
  var initialPos = {
    index: 0,
    line: 1,
    column: 1
  };
  var runParserT = function(dictMonadRec) {
    var map31 = map(dictMonadRec.Monad0().Bind1().Apply0().Functor0());
    var runParserT$prime1 = runParserT$prime(dictMonadRec);
    return function(s) {
      return function(p4) {
        var initialState4 = new ParseState(s, initialPos, false);
        return map31(fst)(runParserT$prime1(initialState4)(p4));
      };
    };
  };
  var runParserT1 = /* @__PURE__ */ runParserT(monadRecIdentity);
  var runParser = function(s) {
    var $281 = runParserT1(s);
    return function($282) {
      return unwrap4($281($282));
    };
  };
  var getParserT = function(state1, v, v1, v2, done) {
    return done(state1, state1);
  };
  var failWithPosition = function(message2) {
    return function(pos) {
      return throwError3(new ParseError(message2, pos));
    };
  };
  var fail2 = function(message2) {
    return bindFlipped8(failWithPosition(message2))(position2);
  };
  var liftMaybe = function(dictMonad) {
    return function(message2) {
      return function(f) {
        if (f instanceof Nothing) {
          return fail2(message2(unit));
        }
        ;
        if (f instanceof Just) {
          return pure10(f.value0);
        }
        ;
        throw new Error("Failed pattern match at Parsing (line 501, column 23 - line 503, column 19): " + [f.constructor.name]);
      };
    };
  };
  var plusParserT = {
    empty: /* @__PURE__ */ fail2("No alternative"),
    Alt0: function() {
      return altParserT;
    }
  };
  var alternativeParserT = {
    Applicative0: function() {
      return applicativeParserT;
    },
    Plus1: function() {
      return plusParserT;
    }
  };

  // output/Parsing.Combinators/index.js
  var alt5 = /* @__PURE__ */ alt(altParserT);
  var pure11 = /* @__PURE__ */ pure(applicativeParserT);
  var map18 = /* @__PURE__ */ map(functorParserT);
  var $$void8 = /* @__PURE__ */ $$void(functorParserT);
  var withErrorMessage = function(p4) {
    return function(msg) {
      return alt5(p4)(fail2("Expected " + msg));
    };
  };
  var optional = function(p4) {
    return alt5($$void8(p4))(pure11(unit));
  };
  var option2 = function(a2) {
    return function(p4) {
      return alt5(p4)(pure11(a2));
    };
  };
  var optionMaybe = function(p4) {
    return option2(Nothing.value)(map18(Just.create)(p4));
  };

  // output/Data.Array.NonEmpty.Internal/foreign.js
  var traverse1Impl = function() {
    function Cont(fn) {
      this.fn = fn;
    }
    var emptyList = {};
    var ConsCell = function(head6, tail2) {
      this.head = head6;
      this.tail = tail2;
    };
    function finalCell(head6) {
      return new ConsCell(head6, emptyList);
    }
    function consList(x) {
      return function(xs) {
        return new ConsCell(x, xs);
      };
    }
    function listToArray(list) {
      var arr = [];
      var xs = list;
      while (xs !== emptyList) {
        arr.push(xs.head);
        xs = xs.tail;
      }
      return arr;
    }
    return function(apply4) {
      return function(map31) {
        return function(f) {
          var buildFrom = function(x, ys) {
            return apply4(map31(consList)(f(x)))(ys);
          };
          var go2 = function(acc, currentLen, xs) {
            if (currentLen === 0) {
              return acc;
            } else {
              var last3 = xs[currentLen - 1];
              return new Cont(function() {
                var built = go2(buildFrom(last3, acc), currentLen - 1, xs);
                return built;
              });
            }
          };
          return function(array) {
            var acc = map31(finalCell)(f(array[array.length - 1]));
            var result = go2(acc, array.length - 1, array);
            while (result instanceof Cont) {
              result = result.fn();
            }
            return map31(listToArray)(result);
          };
        };
      };
    };
  }();

  // output/Data.Array.NonEmpty.Internal/index.js
  var NonEmptyArray = function(x) {
    return x;
  };

  // output/Data.Array.NonEmpty/index.js
  var unsafeFromArrayF = unsafeCoerce2;
  var unsafeFromArray = NonEmptyArray;
  var toArray = function(v) {
    return v;
  };
  var updateAt3 = function(i2) {
    return function(x) {
      var $106 = updateAt(i2)(x);
      return function($107) {
        return unsafeFromArrayF($106(toArray($107)));
      };
    };
  };
  var snoc5 = function(xs) {
    return function(x) {
      return unsafeFromArray(snoc(toArray(xs))(x));
    };
  };
  var singleton7 = function($110) {
    return unsafeFromArray(singleton4($110));
  };
  var fromArray = function(xs) {
    if (length4(xs) > 0) {
      return new Just(unsafeFromArray(xs));
    }
    ;
    if (otherwise) {
      return Nothing.value;
    }
    ;
    throw new Error("Failed pattern match at Data.Array.NonEmpty (line 161, column 1 - line 161, column 58): " + [xs.constructor.name]);
  };
  var adaptAny = function(f) {
    return function($128) {
      return f(toArray($128));
    };
  };
  var deleteAt2 = function(i2) {
    return adaptAny(deleteAt(i2));
  };
  var index4 = /* @__PURE__ */ adaptAny(index2);
  var length8 = /* @__PURE__ */ adaptAny(length4);
  var unsafeAdapt = function(f) {
    var $129 = adaptAny(f);
    return function($130) {
      return unsafeFromArray($129($130));
    };
  };
  var mapWithIndex3 = function(f) {
    return unsafeAdapt(mapWithIndex2(f));
  };

  // output/Data.String.CodePoints/foreign.js
  var hasArrayFrom = typeof Array.from === "function";
  var hasStringIterator = typeof Symbol !== "undefined" && Symbol != null && typeof Symbol.iterator !== "undefined" && typeof String.prototype[Symbol.iterator] === "function";
  var hasFromCodePoint = typeof String.prototype.fromCodePoint === "function";
  var hasCodePointAt = typeof String.prototype.codePointAt === "function";
  var _unsafeCodePointAt0 = function(fallback) {
    return hasCodePointAt ? function(str) {
      return str.codePointAt(0);
    } : fallback;
  };
  var _codePointAt = function(fallback) {
    return function(Just2) {
      return function(Nothing2) {
        return function(unsafeCodePointAt02) {
          return function(index5) {
            return function(str) {
              var length13 = str.length;
              if (index5 < 0 || index5 >= length13)
                return Nothing2;
              if (hasStringIterator) {
                var iter = str[Symbol.iterator]();
                for (var i2 = index5; ; --i2) {
                  var o = iter.next();
                  if (o.done)
                    return Nothing2;
                  if (i2 === 0)
                    return Just2(unsafeCodePointAt02(o.value));
                }
              }
              return fallback(index5)(str);
            };
          };
        };
      };
    };
  };
  var _fromCodePointArray = function(singleton10) {
    return hasFromCodePoint ? function(cps) {
      if (cps.length < 1e4) {
        return String.fromCodePoint.apply(String, cps);
      }
      return cps.map(singleton10).join("");
    } : function(cps) {
      return cps.map(singleton10).join("");
    };
  };
  var _singleton = function(fallback) {
    return hasFromCodePoint ? String.fromCodePoint : fallback;
  };
  var _take = function(fallback) {
    return function(n) {
      if (hasStringIterator) {
        return function(str) {
          var accum = "";
          var iter = str[Symbol.iterator]();
          for (var i2 = 0; i2 < n; ++i2) {
            var o = iter.next();
            if (o.done)
              return accum;
            accum += o.value;
          }
          return accum;
        };
      }
      return fallback(n);
    };
  };
  var _toCodePointArray = function(fallback) {
    return function(unsafeCodePointAt02) {
      if (hasArrayFrom) {
        return function(str) {
          return Array.from(str, unsafeCodePointAt02);
        };
      }
      return fallback;
    };
  };

  // output/Data.String.CodePoints/index.js
  var $runtime_lazy8 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var fromEnum2 = /* @__PURE__ */ fromEnum(boundedEnumChar);
  var map19 = /* @__PURE__ */ map(functorMaybe);
  var unfoldr2 = /* @__PURE__ */ unfoldr(unfoldableArray);
  var div3 = /* @__PURE__ */ div(euclideanRingInt);
  var mod2 = /* @__PURE__ */ mod(euclideanRingInt);
  var compare2 = /* @__PURE__ */ compare(ordInt);
  var CodePoint = function(x) {
    return x;
  };
  var unsurrogate = function(lead) {
    return function(trail) {
      return (((lead - 55296 | 0) * 1024 | 0) + (trail - 56320 | 0) | 0) + 65536 | 0;
    };
  };
  var isTrail = function(cu) {
    return 56320 <= cu && cu <= 57343;
  };
  var isLead = function(cu) {
    return 55296 <= cu && cu <= 56319;
  };
  var uncons5 = function(s) {
    var v = length6(s);
    if (v === 0) {
      return Nothing.value;
    }
    ;
    if (v === 1) {
      return new Just({
        head: fromEnum2(charAt(0)(s)),
        tail: ""
      });
    }
    ;
    var cu1 = fromEnum2(charAt(1)(s));
    var cu0 = fromEnum2(charAt(0)(s));
    var $43 = isLead(cu0) && isTrail(cu1);
    if ($43) {
      return new Just({
        head: unsurrogate(cu0)(cu1),
        tail: drop3(2)(s)
      });
    }
    ;
    return new Just({
      head: cu0,
      tail: drop3(1)(s)
    });
  };
  var unconsButWithTuple = function(s) {
    return map19(function(v) {
      return new Tuple(v.head, v.tail);
    })(uncons5(s));
  };
  var toCodePointArrayFallback = function(s) {
    return unfoldr2(unconsButWithTuple)(s);
  };
  var unsafeCodePointAt0Fallback = function(s) {
    var cu0 = fromEnum2(charAt(0)(s));
    var $47 = isLead(cu0) && length6(s) > 1;
    if ($47) {
      var cu1 = fromEnum2(charAt(1)(s));
      var $48 = isTrail(cu1);
      if ($48) {
        return unsurrogate(cu0)(cu1);
      }
      ;
      return cu0;
    }
    ;
    return cu0;
  };
  var unsafeCodePointAt0 = /* @__PURE__ */ _unsafeCodePointAt0(unsafeCodePointAt0Fallback);
  var toCodePointArray = /* @__PURE__ */ _toCodePointArray(toCodePointArrayFallback)(unsafeCodePointAt0);
  var length9 = function($74) {
    return length4(toCodePointArray($74));
  };
  var fromCharCode2 = /* @__PURE__ */ function() {
    var $75 = toEnumWithDefaults(boundedEnumChar)(bottom(boundedChar))(top(boundedChar));
    return function($76) {
      return singleton6($75($76));
    };
  }();
  var singletonFallback = function(v) {
    if (v <= 65535) {
      return fromCharCode2(v);
    }
    ;
    var lead = div3(v - 65536 | 0)(1024) + 55296 | 0;
    var trail = mod2(v - 65536 | 0)(1024) + 56320 | 0;
    return fromCharCode2(lead) + fromCharCode2(trail);
  };
  var fromCodePointArray = /* @__PURE__ */ _fromCodePointArray(singletonFallback);
  var singleton8 = /* @__PURE__ */ _singleton(singletonFallback);
  var takeFallback = function(v) {
    return function(v1) {
      if (v < 1) {
        return "";
      }
      ;
      var v2 = uncons5(v1);
      if (v2 instanceof Just) {
        return singleton8(v2.value0.head) + takeFallback(v - 1 | 0)(v2.value0.tail);
      }
      ;
      return v1;
    };
  };
  var take4 = /* @__PURE__ */ _take(takeFallback);
  var splitAt3 = function(i2) {
    return function(s) {
      var before = take4(i2)(s);
      return {
        before,
        after: drop3(length6(before))(s)
      };
    };
  };
  var eqCodePoint = {
    eq: function(x) {
      return function(y) {
        return x === y;
      };
    }
  };
  var ordCodePoint = {
    compare: function(x) {
      return function(y) {
        return compare2(x)(y);
      };
    },
    Eq0: function() {
      return eqCodePoint;
    }
  };
  var drop4 = function(n) {
    return function(s) {
      return drop3(length6(take4(n)(s)))(s);
    };
  };
  var codePointFromChar = function($77) {
    return CodePoint(fromEnum2($77));
  };
  var codePointAtFallback = function($copy_n) {
    return function($copy_s) {
      var $tco_var_n = $copy_n;
      var $tco_done = false;
      var $tco_result;
      function $tco_loop(n, s) {
        var v = uncons5(s);
        if (v instanceof Just) {
          var $66 = n === 0;
          if ($66) {
            $tco_done = true;
            return new Just(v.value0.head);
          }
          ;
          $tco_var_n = n - 1 | 0;
          $copy_s = v.value0.tail;
          return;
        }
        ;
        $tco_done = true;
        return Nothing.value;
      }
      ;
      while (!$tco_done) {
        $tco_result = $tco_loop($tco_var_n, $copy_s);
      }
      ;
      return $tco_result;
    };
  };
  var codePointAt = function(v) {
    return function(v1) {
      if (v < 0) {
        return Nothing.value;
      }
      ;
      if (v === 0 && v1 === "") {
        return Nothing.value;
      }
      ;
      if (v === 0) {
        return new Just(unsafeCodePointAt0(v1));
      }
      ;
      return _codePointAt(codePointAtFallback)(Just.create)(Nothing.value)(unsafeCodePointAt0)(v)(v1);
    };
  };
  var boundedCodePoint = {
    bottom: 0,
    top: 1114111,
    Ord0: function() {
      return ordCodePoint;
    }
  };
  var boundedEnumCodePoint = /* @__PURE__ */ function() {
    return {
      cardinality: 1114111 + 1 | 0,
      fromEnum: function(v) {
        return v;
      },
      toEnum: function(n) {
        if (n >= 0 && n <= 1114111) {
          return new Just(n);
        }
        ;
        if (otherwise) {
          return Nothing.value;
        }
        ;
        throw new Error("Failed pattern match at Data.String.CodePoints (line 63, column 1 - line 68, column 26): " + [n.constructor.name]);
      },
      Bounded0: function() {
        return boundedCodePoint;
      },
      Enum1: function() {
        return $lazy_enumCodePoint(0);
      }
    };
  }();
  var $lazy_enumCodePoint = /* @__PURE__ */ $runtime_lazy8("enumCodePoint", "Data.String.CodePoints", function() {
    return {
      succ: defaultSucc(toEnum(boundedEnumCodePoint))(fromEnum(boundedEnumCodePoint)),
      pred: defaultPred(toEnum(boundedEnumCodePoint))(fromEnum(boundedEnumCodePoint)),
      Ord0: function() {
        return ordCodePoint;
      }
    };
  });

  // output/Parsing.String/index.js
  var fromEnum3 = /* @__PURE__ */ fromEnum(boundedEnumCodePoint);
  var mod3 = /* @__PURE__ */ mod(euclideanRingInt);
  var fromJust5 = /* @__PURE__ */ fromJust();
  var toEnum2 = /* @__PURE__ */ toEnum(boundedEnumChar);
  var show2 = /* @__PURE__ */ show(showInt);
  var show1 = /* @__PURE__ */ show(showString);
  var show22 = /* @__PURE__ */ show(showChar);
  var updatePosSingle = function(v) {
    return function(cp) {
      return function(after) {
        var v1 = fromEnum3(cp);
        if (v1 === 10) {
          return {
            index: v.index + 1 | 0,
            line: v.line + 1 | 0,
            column: 1
          };
        }
        ;
        if (v1 === 13) {
          var v2 = codePointAt(0)(after);
          if (v2 instanceof Just && fromEnum3(v2.value0) === 10) {
            return {
              index: v.index + 1 | 0,
              line: v.line,
              column: v.column
            };
          }
          ;
          return {
            index: v.index + 1 | 0,
            line: v.line + 1 | 0,
            column: 1
          };
        }
        ;
        if (v1 === 9) {
          return {
            index: v.index + 1 | 0,
            line: v.line,
            column: (v.column + 8 | 0) - mod3(v.column - 1 | 0)(8) | 0
          };
        }
        ;
        return {
          index: v.index + 1 | 0,
          line: v.line,
          column: v.column + 1 | 0
        };
      };
    };
  };
  var updatePosString = function($copy_pos) {
    return function($copy_before) {
      return function($copy_after) {
        var $tco_var_pos = $copy_pos;
        var $tco_var_before = $copy_before;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(pos, before, after) {
          var v = uncons5(before);
          if (v instanceof Nothing) {
            $tco_done = true;
            return pos;
          }
          ;
          if (v instanceof Just) {
            var newPos = function() {
              if ($$null2(v.value0.tail)) {
                return updatePosSingle(pos)(v.value0.head)(after);
              }
              ;
              if (otherwise) {
                return updatePosSingle(pos)(v.value0.head)(v.value0.tail);
              }
              ;
              throw new Error("Failed pattern match at Parsing.String (line 165, column 7 - line 167, column 52): " + []);
            }();
            $tco_var_pos = newPos;
            $tco_var_before = v.value0.tail;
            $copy_after = after;
            return;
          }
          ;
          throw new Error("Failed pattern match at Parsing.String (line 161, column 36 - line 168, column 38): " + [v.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_pos, $tco_var_before, $copy_after);
        }
        ;
        return $tco_result;
      };
    };
  };
  var satisfy = function(f) {
    return mkFn5(function(v) {
      return function(v1) {
        return function(v2) {
          return function($$throw2) {
            return function(done) {
              var v3 = uncons5(v.value0);
              if (v3 instanceof Nothing) {
                return $$throw2(v, new ParseError("Unexpected EOF", v.value1));
              }
              ;
              if (v3 instanceof Just) {
                var cp = fromEnum3(v3.value0.head);
                var $85 = cp < 0 || cp > 65535;
                if ($85) {
                  return $$throw2(v, new ParseError("Expected Char", v.value1));
                }
                ;
                var ch = fromJust5(toEnum2(cp));
                var $86 = f(ch);
                if ($86) {
                  return done(new ParseState(v3.value0.tail, updatePosSingle(v.value1)(v3.value0.head)(v3.value0.tail), true), ch);
                }
                ;
                return $$throw2(v, new ParseError("Predicate unsatisfied", v.value1));
              }
              ;
              throw new Error("Failed pattern match at Parsing.String (line 114, column 7 - line 129, column 75): " + [v3.constructor.name]);
            };
          };
        };
      };
    });
  };
  var consumeWith = function(f) {
    return mkFn5(function(v) {
      return function(v1) {
        return function(v2) {
          return function($$throw2) {
            return function(done) {
              var v3 = f(v.value0);
              if (v3 instanceof Left) {
                return $$throw2(v, new ParseError(v3.value0, v.value1));
              }
              ;
              if (v3 instanceof Right) {
                return done(new ParseState(v3.value0.remainder, updatePosString(v.value1)(v3.value0.consumed)(v3.value0.remainder), !$$null2(v3.value0.consumed)), v3.value0.value);
              }
              ;
              throw new Error("Failed pattern match at Parsing.String (line 286, column 7 - line 290, column 121): " + [v3.constructor.name]);
            };
          };
        };
      };
    });
  };
  var rest = /* @__PURE__ */ consumeWith(function(consumed) {
    return new Right({
      value: consumed,
      consumed,
      remainder: ""
    });
  });
  var string = function(str) {
    return consumeWith(function(input3) {
      var v = stripPrefix(str)(input3);
      if (v instanceof Just) {
        return new Right({
          value: str,
          consumed: str,
          remainder: v.value0
        });
      }
      ;
      return new Left("Expected " + show1(str));
    });
  };
  var takeN = function(n) {
    return consumeWith(function(input3) {
      var v = splitAt3(n)(input3);
      var $153 = length9(v.before) === n;
      if ($153) {
        return new Right({
          value: v.before,
          consumed: v.before,
          remainder: v.after
        });
      }
      ;
      return new Left("Could not take " + (show2(n) + " characters"));
    });
  };
  var $$char = function(c) {
    return withErrorMessage(satisfy(function(v) {
      return v === c;
    }))(show22(c));
  };

  // output/Data.Char/index.js
  var toCharCode2 = /* @__PURE__ */ fromEnum(boundedEnumChar);
  var fromCharCode3 = /* @__PURE__ */ toEnum(boundedEnumChar);

  // output/Data.CodePoint.Unicode.Internal/index.js
  var unsafeIndex2 = /* @__PURE__ */ unsafeIndex();
  var elemIndex2 = /* @__PURE__ */ elemIndex(eqInt);
  var NUMCAT_LU = /* @__PURE__ */ function() {
    function NUMCAT_LU2() {
    }
    ;
    NUMCAT_LU2.value = new NUMCAT_LU2();
    return NUMCAT_LU2;
  }();
  var NUMCAT_LL = /* @__PURE__ */ function() {
    function NUMCAT_LL2() {
    }
    ;
    NUMCAT_LL2.value = new NUMCAT_LL2();
    return NUMCAT_LL2;
  }();
  var NUMCAT_LT = /* @__PURE__ */ function() {
    function NUMCAT_LT2() {
    }
    ;
    NUMCAT_LT2.value = new NUMCAT_LT2();
    return NUMCAT_LT2;
  }();
  var NUMCAT_LM = /* @__PURE__ */ function() {
    function NUMCAT_LM2() {
    }
    ;
    NUMCAT_LM2.value = new NUMCAT_LM2();
    return NUMCAT_LM2;
  }();
  var NUMCAT_LO = /* @__PURE__ */ function() {
    function NUMCAT_LO2() {
    }
    ;
    NUMCAT_LO2.value = new NUMCAT_LO2();
    return NUMCAT_LO2;
  }();
  var NUMCAT_MN = /* @__PURE__ */ function() {
    function NUMCAT_MN2() {
    }
    ;
    NUMCAT_MN2.value = new NUMCAT_MN2();
    return NUMCAT_MN2;
  }();
  var NUMCAT_MC = /* @__PURE__ */ function() {
    function NUMCAT_MC2() {
    }
    ;
    NUMCAT_MC2.value = new NUMCAT_MC2();
    return NUMCAT_MC2;
  }();
  var NUMCAT_ME = /* @__PURE__ */ function() {
    function NUMCAT_ME2() {
    }
    ;
    NUMCAT_ME2.value = new NUMCAT_ME2();
    return NUMCAT_ME2;
  }();
  var NUMCAT_ND = /* @__PURE__ */ function() {
    function NUMCAT_ND2() {
    }
    ;
    NUMCAT_ND2.value = new NUMCAT_ND2();
    return NUMCAT_ND2;
  }();
  var NUMCAT_NL = /* @__PURE__ */ function() {
    function NUMCAT_NL2() {
    }
    ;
    NUMCAT_NL2.value = new NUMCAT_NL2();
    return NUMCAT_NL2;
  }();
  var NUMCAT_NO = /* @__PURE__ */ function() {
    function NUMCAT_NO2() {
    }
    ;
    NUMCAT_NO2.value = new NUMCAT_NO2();
    return NUMCAT_NO2;
  }();
  var NUMCAT_PC = /* @__PURE__ */ function() {
    function NUMCAT_PC2() {
    }
    ;
    NUMCAT_PC2.value = new NUMCAT_PC2();
    return NUMCAT_PC2;
  }();
  var NUMCAT_PD = /* @__PURE__ */ function() {
    function NUMCAT_PD2() {
    }
    ;
    NUMCAT_PD2.value = new NUMCAT_PD2();
    return NUMCAT_PD2;
  }();
  var NUMCAT_PS = /* @__PURE__ */ function() {
    function NUMCAT_PS2() {
    }
    ;
    NUMCAT_PS2.value = new NUMCAT_PS2();
    return NUMCAT_PS2;
  }();
  var NUMCAT_PE = /* @__PURE__ */ function() {
    function NUMCAT_PE2() {
    }
    ;
    NUMCAT_PE2.value = new NUMCAT_PE2();
    return NUMCAT_PE2;
  }();
  var NUMCAT_PI = /* @__PURE__ */ function() {
    function NUMCAT_PI2() {
    }
    ;
    NUMCAT_PI2.value = new NUMCAT_PI2();
    return NUMCAT_PI2;
  }();
  var NUMCAT_PF = /* @__PURE__ */ function() {
    function NUMCAT_PF2() {
    }
    ;
    NUMCAT_PF2.value = new NUMCAT_PF2();
    return NUMCAT_PF2;
  }();
  var NUMCAT_PO = /* @__PURE__ */ function() {
    function NUMCAT_PO2() {
    }
    ;
    NUMCAT_PO2.value = new NUMCAT_PO2();
    return NUMCAT_PO2;
  }();
  var NUMCAT_SM = /* @__PURE__ */ function() {
    function NUMCAT_SM2() {
    }
    ;
    NUMCAT_SM2.value = new NUMCAT_SM2();
    return NUMCAT_SM2;
  }();
  var NUMCAT_SC = /* @__PURE__ */ function() {
    function NUMCAT_SC2() {
    }
    ;
    NUMCAT_SC2.value = new NUMCAT_SC2();
    return NUMCAT_SC2;
  }();
  var NUMCAT_SK = /* @__PURE__ */ function() {
    function NUMCAT_SK2() {
    }
    ;
    NUMCAT_SK2.value = new NUMCAT_SK2();
    return NUMCAT_SK2;
  }();
  var NUMCAT_SO = /* @__PURE__ */ function() {
    function NUMCAT_SO2() {
    }
    ;
    NUMCAT_SO2.value = new NUMCAT_SO2();
    return NUMCAT_SO2;
  }();
  var NUMCAT_ZS = /* @__PURE__ */ function() {
    function NUMCAT_ZS2() {
    }
    ;
    NUMCAT_ZS2.value = new NUMCAT_ZS2();
    return NUMCAT_ZS2;
  }();
  var NUMCAT_ZL = /* @__PURE__ */ function() {
    function NUMCAT_ZL2() {
    }
    ;
    NUMCAT_ZL2.value = new NUMCAT_ZL2();
    return NUMCAT_ZL2;
  }();
  var NUMCAT_ZP = /* @__PURE__ */ function() {
    function NUMCAT_ZP2() {
    }
    ;
    NUMCAT_ZP2.value = new NUMCAT_ZP2();
    return NUMCAT_ZP2;
  }();
  var NUMCAT_CC = /* @__PURE__ */ function() {
    function NUMCAT_CC2() {
    }
    ;
    NUMCAT_CC2.value = new NUMCAT_CC2();
    return NUMCAT_CC2;
  }();
  var NUMCAT_CF = /* @__PURE__ */ function() {
    function NUMCAT_CF2() {
    }
    ;
    NUMCAT_CF2.value = new NUMCAT_CF2();
    return NUMCAT_CF2;
  }();
  var NUMCAT_CS = /* @__PURE__ */ function() {
    function NUMCAT_CS2() {
    }
    ;
    NUMCAT_CS2.value = new NUMCAT_CS2();
    return NUMCAT_CS2;
  }();
  var NUMCAT_CO = /* @__PURE__ */ function() {
    function NUMCAT_CO2() {
    }
    ;
    NUMCAT_CO2.value = new NUMCAT_CO2();
    return NUMCAT_CO2;
  }();
  var NUMCAT_CN = /* @__PURE__ */ function() {
    function NUMCAT_CN2() {
    }
    ;
    NUMCAT_CN2.value = new NUMCAT_CN2();
    return NUMCAT_CN2;
  }();
  var numLat1Blocks = 63;
  var numBlocks = 3396;
  var gencatZS = 2;
  var rule1 = /* @__PURE__ */ function() {
    return {
      category: gencatZS,
      unicodeCat: NUMCAT_ZS.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatZP = 67108864;
  var rule162 = /* @__PURE__ */ function() {
    return {
      category: gencatZP,
      unicodeCat: NUMCAT_ZP.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatZL = 33554432;
  var rule161 = /* @__PURE__ */ function() {
    return {
      category: gencatZL,
      unicodeCat: NUMCAT_ZL.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatSO = 8192;
  var rule13 = /* @__PURE__ */ function() {
    return {
      category: gencatSO,
      unicodeCat: NUMCAT_SO.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var rule170 = /* @__PURE__ */ function() {
    return {
      category: gencatSO,
      unicodeCat: NUMCAT_SO.value,
      possible: 1,
      updist: 0,
      lowdist: 26,
      titledist: 0
    };
  }();
  var rule171 = /* @__PURE__ */ function() {
    return {
      category: gencatSO,
      unicodeCat: NUMCAT_SO.value,
      possible: 1,
      updist: -26 | 0,
      lowdist: 0,
      titledist: -26 | 0
    };
  }();
  var gencatSM = 64;
  var rule6 = /* @__PURE__ */ function() {
    return {
      category: gencatSM,
      unicodeCat: NUMCAT_SM.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatSK = 1024;
  var rule10 = /* @__PURE__ */ function() {
    return {
      category: gencatSK,
      unicodeCat: NUMCAT_SK.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatSC = 8;
  var rule3 = /* @__PURE__ */ function() {
    return {
      category: gencatSC,
      unicodeCat: NUMCAT_SC.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatPS = 16;
  var rule4 = /* @__PURE__ */ function() {
    return {
      category: gencatPS,
      unicodeCat: NUMCAT_PS.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatPO = 4;
  var rule2 = /* @__PURE__ */ function() {
    return {
      category: gencatPO,
      unicodeCat: NUMCAT_PO.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatPI = 32768;
  var rule15 = /* @__PURE__ */ function() {
    return {
      category: gencatPI,
      unicodeCat: NUMCAT_PI.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatPF = 262144;
  var rule19 = /* @__PURE__ */ function() {
    return {
      category: gencatPF,
      unicodeCat: NUMCAT_PF.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatPE = 32;
  var rule5 = /* @__PURE__ */ function() {
    return {
      category: gencatPE,
      unicodeCat: NUMCAT_PE.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatPD = 128;
  var rule7 = /* @__PURE__ */ function() {
    return {
      category: gencatPD,
      unicodeCat: NUMCAT_PD.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatPC = 2048;
  var rule11 = /* @__PURE__ */ function() {
    return {
      category: gencatPC,
      unicodeCat: NUMCAT_PC.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatNO = 131072;
  var rule17 = /* @__PURE__ */ function() {
    return {
      category: gencatNO,
      unicodeCat: NUMCAT_NO.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatNL = 16777216;
  var rule128 = /* @__PURE__ */ function() {
    return {
      category: gencatNL,
      unicodeCat: NUMCAT_NL.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var rule168 = /* @__PURE__ */ function() {
    return {
      category: gencatNL,
      unicodeCat: NUMCAT_NL.value,
      possible: 1,
      updist: 0,
      lowdist: 16,
      titledist: 0
    };
  }();
  var rule169 = /* @__PURE__ */ function() {
    return {
      category: gencatNL,
      unicodeCat: NUMCAT_NL.value,
      possible: 1,
      updist: -16 | 0,
      lowdist: 0,
      titledist: -16 | 0
    };
  }();
  var gencatND = 256;
  var rule8 = /* @__PURE__ */ function() {
    return {
      category: gencatND,
      unicodeCat: NUMCAT_ND.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatMN = 2097152;
  var rule92 = /* @__PURE__ */ function() {
    return {
      category: gencatMN,
      unicodeCat: NUMCAT_MN.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var rule93 = /* @__PURE__ */ function() {
    return {
      category: gencatMN,
      unicodeCat: NUMCAT_MN.value,
      possible: 1,
      updist: 84,
      lowdist: 0,
      titledist: 84
    };
  }();
  var gencatME = 4194304;
  var rule119 = /* @__PURE__ */ function() {
    return {
      category: gencatME,
      unicodeCat: NUMCAT_ME.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatMC = 8388608;
  var rule124 = /* @__PURE__ */ function() {
    return {
      category: gencatMC,
      unicodeCat: NUMCAT_MC.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatLU = 512;
  var nullrule = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_CN.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var rule104 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 8,
      titledist: 0
    };
  }();
  var rule107 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var rule115 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -60 | 0,
      titledist: 0
    };
  }();
  var rule117 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -7 | 0,
      titledist: 0
    };
  }();
  var rule118 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 80,
      titledist: 0
    };
  }();
  var rule120 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 15,
      titledist: 0
    };
  }();
  var rule122 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 48,
      titledist: 0
    };
  }();
  var rule125 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 7264,
      titledist: 0
    };
  }();
  var rule127 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 38864,
      titledist: 0
    };
  }();
  var rule137 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -3008 | 0,
      titledist: 0
    };
  }();
  var rule142 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -7615 | 0,
      titledist: 0
    };
  }();
  var rule144 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -8 | 0,
      titledist: 0
    };
  }();
  var rule153 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -74 | 0,
      titledist: 0
    };
  }();
  var rule156 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -86 | 0,
      titledist: 0
    };
  }();
  var rule157 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -100 | 0,
      titledist: 0
    };
  }();
  var rule158 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -112 | 0,
      titledist: 0
    };
  }();
  var rule159 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -128 | 0,
      titledist: 0
    };
  }();
  var rule160 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -126 | 0,
      titledist: 0
    };
  }();
  var rule163 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -7517 | 0,
      titledist: 0
    };
  }();
  var rule164 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -8383 | 0,
      titledist: 0
    };
  }();
  var rule165 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -8262 | 0,
      titledist: 0
    };
  }();
  var rule166 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 28,
      titledist: 0
    };
  }();
  var rule172 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -10743 | 0,
      titledist: 0
    };
  }();
  var rule173 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -3814 | 0,
      titledist: 0
    };
  }();
  var rule174 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -10727 | 0,
      titledist: 0
    };
  }();
  var rule177 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -10780 | 0,
      titledist: 0
    };
  }();
  var rule178 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -10749 | 0,
      titledist: 0
    };
  }();
  var rule179 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -10783 | 0,
      titledist: 0
    };
  }();
  var rule180 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -10782 | 0,
      titledist: 0
    };
  }();
  var rule181 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -10815 | 0,
      titledist: 0
    };
  }();
  var rule183 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -35332 | 0,
      titledist: 0
    };
  }();
  var rule184 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -42280 | 0,
      titledist: 0
    };
  }();
  var rule186 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -42308 | 0,
      titledist: 0
    };
  }();
  var rule187 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -42319 | 0,
      titledist: 0
    };
  }();
  var rule188 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -42315 | 0,
      titledist: 0
    };
  }();
  var rule189 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -42305 | 0,
      titledist: 0
    };
  }();
  var rule190 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -42258 | 0,
      titledist: 0
    };
  }();
  var rule191 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -42282 | 0,
      titledist: 0
    };
  }();
  var rule192 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -42261 | 0,
      titledist: 0
    };
  }();
  var rule193 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 928,
      titledist: 0
    };
  }();
  var rule194 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -48 | 0,
      titledist: 0
    };
  }();
  var rule195 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -42307 | 0,
      titledist: 0
    };
  }();
  var rule196 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -35384 | 0,
      titledist: 0
    };
  }();
  var rule201 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 40,
      titledist: 0
    };
  }();
  var rule203 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 34,
      titledist: 0
    };
  }();
  var rule22 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 1,
      titledist: 0
    };
  }();
  var rule24 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -199 | 0,
      titledist: 0
    };
  }();
  var rule26 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -121 | 0,
      titledist: 0
    };
  }();
  var rule29 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 210,
      titledist: 0
    };
  }();
  var rule30 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 206,
      titledist: 0
    };
  }();
  var rule31 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 205,
      titledist: 0
    };
  }();
  var rule32 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 79,
      titledist: 0
    };
  }();
  var rule33 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 202,
      titledist: 0
    };
  }();
  var rule34 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 203,
      titledist: 0
    };
  }();
  var rule35 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 207,
      titledist: 0
    };
  }();
  var rule37 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 211,
      titledist: 0
    };
  }();
  var rule38 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 209,
      titledist: 0
    };
  }();
  var rule40 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 213,
      titledist: 0
    };
  }();
  var rule42 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 214,
      titledist: 0
    };
  }();
  var rule43 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 218,
      titledist: 0
    };
  }();
  var rule44 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 217,
      titledist: 0
    };
  }();
  var rule45 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 219,
      titledist: 0
    };
  }();
  var rule47 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 2,
      titledist: 1
    };
  }();
  var rule51 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -97 | 0,
      titledist: 0
    };
  }();
  var rule52 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -56 | 0,
      titledist: 0
    };
  }();
  var rule53 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -130 | 0,
      titledist: 0
    };
  }();
  var rule54 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 10795,
      titledist: 0
    };
  }();
  var rule55 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -163 | 0,
      titledist: 0
    };
  }();
  var rule56 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 10792,
      titledist: 0
    };
  }();
  var rule58 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: -195 | 0,
      titledist: 0
    };
  }();
  var rule59 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 69,
      titledist: 0
    };
  }();
  var rule60 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 71,
      titledist: 0
    };
  }();
  var rule9 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 32,
      titledist: 0
    };
  }();
  var rule94 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 116,
      titledist: 0
    };
  }();
  var rule95 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 38,
      titledist: 0
    };
  }();
  var rule96 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 37,
      titledist: 0
    };
  }();
  var rule97 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 64,
      titledist: 0
    };
  }();
  var rule98 = /* @__PURE__ */ function() {
    return {
      category: gencatLU,
      unicodeCat: NUMCAT_LU.value,
      possible: 1,
      updist: 0,
      lowdist: 63,
      titledist: 0
    };
  }();
  var gencatLT = 524288;
  var rule151 = /* @__PURE__ */ function() {
    return {
      category: gencatLT,
      unicodeCat: NUMCAT_LT.value,
      possible: 1,
      updist: 0,
      lowdist: -8 | 0,
      titledist: 0
    };
  }();
  var rule154 = /* @__PURE__ */ function() {
    return {
      category: gencatLT,
      unicodeCat: NUMCAT_LT.value,
      possible: 1,
      updist: 0,
      lowdist: -9 | 0,
      titledist: 0
    };
  }();
  var rule48 = /* @__PURE__ */ function() {
    return {
      category: gencatLT,
      unicodeCat: NUMCAT_LT.value,
      possible: 1,
      updist: -1 | 0,
      lowdist: 1,
      titledist: 0
    };
  }();
  var gencatLO = 16384;
  var rule14 = /* @__PURE__ */ function() {
    return {
      category: gencatLO,
      unicodeCat: NUMCAT_LO.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatLM = 1048576;
  var rule91 = /* @__PURE__ */ function() {
    return {
      category: gencatLM,
      unicodeCat: NUMCAT_LM.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatLL = 4096;
  var rule100 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -37 | 0,
      lowdist: 0,
      titledist: -37 | 0
    };
  }();
  var rule101 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -31 | 0,
      lowdist: 0,
      titledist: -31 | 0
    };
  }();
  var rule102 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -64 | 0,
      lowdist: 0,
      titledist: -64 | 0
    };
  }();
  var rule103 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -63 | 0,
      lowdist: 0,
      titledist: -63 | 0
    };
  }();
  var rule105 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -62 | 0,
      lowdist: 0,
      titledist: -62 | 0
    };
  }();
  var rule106 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -57 | 0,
      lowdist: 0,
      titledist: -57 | 0
    };
  }();
  var rule108 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -47 | 0,
      lowdist: 0,
      titledist: -47 | 0
    };
  }();
  var rule109 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -54 | 0,
      lowdist: 0,
      titledist: -54 | 0
    };
  }();
  var rule110 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -8 | 0,
      lowdist: 0,
      titledist: -8 | 0
    };
  }();
  var rule111 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -86 | 0,
      lowdist: 0,
      titledist: -86 | 0
    };
  }();
  var rule112 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -80 | 0,
      lowdist: 0,
      titledist: -80 | 0
    };
  }();
  var rule113 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 7,
      lowdist: 0,
      titledist: 7
    };
  }();
  var rule114 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -116 | 0,
      lowdist: 0,
      titledist: -116 | 0
    };
  }();
  var rule116 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -96 | 0,
      lowdist: 0,
      titledist: -96 | 0
    };
  }();
  var rule12 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -32 | 0,
      lowdist: 0,
      titledist: -32 | 0
    };
  }();
  var rule121 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -15 | 0,
      lowdist: 0,
      titledist: -15 | 0
    };
  }();
  var rule123 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -48 | 0,
      lowdist: 0,
      titledist: -48 | 0
    };
  }();
  var rule126 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 3008,
      lowdist: 0,
      titledist: 0
    };
  }();
  var rule129 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -6254 | 0,
      lowdist: 0,
      titledist: -6254 | 0
    };
  }();
  var rule130 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -6253 | 0,
      lowdist: 0,
      titledist: -6253 | 0
    };
  }();
  var rule131 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -6244 | 0,
      lowdist: 0,
      titledist: -6244 | 0
    };
  }();
  var rule132 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -6242 | 0,
      lowdist: 0,
      titledist: -6242 | 0
    };
  }();
  var rule133 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -6243 | 0,
      lowdist: 0,
      titledist: -6243 | 0
    };
  }();
  var rule134 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -6236 | 0,
      lowdist: 0,
      titledist: -6236 | 0
    };
  }();
  var rule135 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -6181 | 0,
      lowdist: 0,
      titledist: -6181 | 0
    };
  }();
  var rule136 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 35266,
      lowdist: 0,
      titledist: 35266
    };
  }();
  var rule138 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 35332,
      lowdist: 0,
      titledist: 35332
    };
  }();
  var rule139 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 3814,
      lowdist: 0,
      titledist: 3814
    };
  }();
  var rule140 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 35384,
      lowdist: 0,
      titledist: 35384
    };
  }();
  var rule141 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -59 | 0,
      lowdist: 0,
      titledist: -59 | 0
    };
  }();
  var rule143 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 8,
      lowdist: 0,
      titledist: 8
    };
  }();
  var rule145 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 74,
      lowdist: 0,
      titledist: 74
    };
  }();
  var rule146 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 86,
      lowdist: 0,
      titledist: 86
    };
  }();
  var rule147 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 100,
      lowdist: 0,
      titledist: 100
    };
  }();
  var rule148 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 128,
      lowdist: 0,
      titledist: 128
    };
  }();
  var rule149 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 112,
      lowdist: 0,
      titledist: 112
    };
  }();
  var rule150 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 126,
      lowdist: 0,
      titledist: 126
    };
  }();
  var rule152 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 9,
      lowdist: 0,
      titledist: 9
    };
  }();
  var rule155 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -7205 | 0,
      lowdist: 0,
      titledist: -7205 | 0
    };
  }();
  var rule167 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -28 | 0,
      lowdist: 0,
      titledist: -28 | 0
    };
  }();
  var rule175 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -10795 | 0,
      lowdist: 0,
      titledist: -10795 | 0
    };
  }();
  var rule176 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -10792 | 0,
      lowdist: 0,
      titledist: -10792 | 0
    };
  }();
  var rule18 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 743,
      lowdist: 0,
      titledist: 743
    };
  }();
  var rule182 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -7264 | 0,
      lowdist: 0,
      titledist: -7264 | 0
    };
  }();
  var rule185 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 48,
      lowdist: 0,
      titledist: 48
    };
  }();
  var rule197 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -928 | 0,
      lowdist: 0,
      titledist: -928 | 0
    };
  }();
  var rule198 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -38864 | 0,
      lowdist: 0,
      titledist: -38864 | 0
    };
  }();
  var rule20 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var rule202 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -40 | 0,
      lowdist: 0,
      titledist: -40 | 0
    };
  }();
  var rule204 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -34 | 0,
      lowdist: 0,
      titledist: -34 | 0
    };
  }();
  var rule21 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 121,
      lowdist: 0,
      titledist: 121
    };
  }();
  var rule23 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -1 | 0,
      lowdist: 0,
      titledist: -1 | 0
    };
  }();
  var rule25 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -232 | 0,
      lowdist: 0,
      titledist: -232 | 0
    };
  }();
  var rule27 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -300 | 0,
      lowdist: 0,
      titledist: -300 | 0
    };
  }();
  var rule28 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 195,
      lowdist: 0,
      titledist: 195
    };
  }();
  var rule36 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 97,
      lowdist: 0,
      titledist: 97
    };
  }();
  var rule39 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 163,
      lowdist: 0,
      titledist: 163
    };
  }();
  var rule41 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 130,
      lowdist: 0,
      titledist: 130
    };
  }();
  var rule46 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 56,
      lowdist: 0,
      titledist: 56
    };
  }();
  var rule49 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -2 | 0,
      lowdist: 0,
      titledist: -1 | 0
    };
  }();
  var rule50 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -79 | 0,
      lowdist: 0,
      titledist: -79 | 0
    };
  }();
  var rule57 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 10815,
      lowdist: 0,
      titledist: 10815
    };
  }();
  var rule61 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 10783,
      lowdist: 0,
      titledist: 10783
    };
  }();
  var rule62 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 10780,
      lowdist: 0,
      titledist: 10780
    };
  }();
  var rule63 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 10782,
      lowdist: 0,
      titledist: 10782
    };
  }();
  var rule64 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -210 | 0,
      lowdist: 0,
      titledist: -210 | 0
    };
  }();
  var rule65 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -206 | 0,
      lowdist: 0,
      titledist: -206 | 0
    };
  }();
  var rule66 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -205 | 0,
      lowdist: 0,
      titledist: -205 | 0
    };
  }();
  var rule67 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -202 | 0,
      lowdist: 0,
      titledist: -202 | 0
    };
  }();
  var rule68 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -203 | 0,
      lowdist: 0,
      titledist: -203 | 0
    };
  }();
  var rule69 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 42319,
      lowdist: 0,
      titledist: 42319
    };
  }();
  var rule70 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 42315,
      lowdist: 0,
      titledist: 42315
    };
  }();
  var rule71 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -207 | 0,
      lowdist: 0,
      titledist: -207 | 0
    };
  }();
  var rule72 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 42280,
      lowdist: 0,
      titledist: 42280
    };
  }();
  var rule73 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 42308,
      lowdist: 0,
      titledist: 42308
    };
  }();
  var rule74 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -209 | 0,
      lowdist: 0,
      titledist: -209 | 0
    };
  }();
  var rule75 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -211 | 0,
      lowdist: 0,
      titledist: -211 | 0
    };
  }();
  var rule76 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 10743,
      lowdist: 0,
      titledist: 10743
    };
  }();
  var rule77 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 42305,
      lowdist: 0,
      titledist: 42305
    };
  }();
  var rule78 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 10749,
      lowdist: 0,
      titledist: 10749
    };
  }();
  var rule79 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -213 | 0,
      lowdist: 0,
      titledist: -213 | 0
    };
  }();
  var rule80 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -214 | 0,
      lowdist: 0,
      titledist: -214 | 0
    };
  }();
  var rule81 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 10727,
      lowdist: 0,
      titledist: 10727
    };
  }();
  var rule82 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -218 | 0,
      lowdist: 0,
      titledist: -218 | 0
    };
  }();
  var rule83 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 42307,
      lowdist: 0,
      titledist: 42307
    };
  }();
  var rule84 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 42282,
      lowdist: 0,
      titledist: 42282
    };
  }();
  var rule85 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -69 | 0,
      lowdist: 0,
      titledist: -69 | 0
    };
  }();
  var rule86 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -217 | 0,
      lowdist: 0,
      titledist: -217 | 0
    };
  }();
  var rule87 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -71 | 0,
      lowdist: 0,
      titledist: -71 | 0
    };
  }();
  var rule88 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -219 | 0,
      lowdist: 0,
      titledist: -219 | 0
    };
  }();
  var rule89 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 42261,
      lowdist: 0,
      titledist: 42261
    };
  }();
  var rule90 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: 42258,
      lowdist: 0,
      titledist: 42258
    };
  }();
  var rule99 = /* @__PURE__ */ function() {
    return {
      category: gencatLL,
      unicodeCat: NUMCAT_LL.value,
      possible: 1,
      updist: -38 | 0,
      lowdist: 0,
      titledist: -38 | 0
    };
  }();
  var gencatCS = 134217728;
  var rule199 = /* @__PURE__ */ function() {
    return {
      category: gencatCS,
      unicodeCat: NUMCAT_CS.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatCO = 268435456;
  var rule200 = /* @__PURE__ */ function() {
    return {
      category: gencatCO,
      unicodeCat: NUMCAT_CO.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatCF = 65536;
  var rule16 = /* @__PURE__ */ function() {
    return {
      category: gencatCF,
      unicodeCat: NUMCAT_CF.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var gencatCC = 1;
  var rule0 = /* @__PURE__ */ function() {
    return {
      category: gencatCC,
      unicodeCat: NUMCAT_CC.value,
      possible: 0,
      updist: 0,
      lowdist: 0,
      titledist: 0
    };
  }();
  var bsearch = function(a2) {
    return function(array) {
      return function(size5) {
        return function(compare3) {
          var go2 = function($copy_i) {
            return function($copy_k) {
              var $tco_var_i = $copy_i;
              var $tco_done = false;
              var $tco_result;
              function $tco_loop(i2, k) {
                if (i2 > k || i2 >= length4(array)) {
                  $tco_done = true;
                  return Nothing.value;
                }
                ;
                if (otherwise) {
                  var j = floor2(toNumber(i2 + k | 0) / 2);
                  var b2 = unsafeIndex2(array)(j);
                  var v = compare3(a2)(b2);
                  if (v instanceof EQ) {
                    $tco_done = true;
                    return new Just(b2);
                  }
                  ;
                  if (v instanceof GT) {
                    $tco_var_i = j + 1 | 0;
                    $copy_k = k;
                    return;
                  }
                  ;
                  $tco_var_i = i2;
                  $copy_k = j - 1 | 0;
                  return;
                }
                ;
                throw new Error("Failed pattern match at Data.CodePoint.Unicode.Internal (line 5622, column 3 - line 5632, column 30): " + [i2.constructor.name, k.constructor.name]);
              }
              ;
              while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_i, $copy_k);
              }
              ;
              return $tco_result;
            };
          };
          return go2(0)(size5);
        };
      };
    };
  };
  var blkCmp = function(v) {
    return function(v1) {
      if (v.start >= v1.start && v.start < (v1.start + v1.length | 0)) {
        return EQ.value;
      }
      ;
      if (v.start > v1.start) {
        return GT.value;
      }
      ;
      if (otherwise) {
        return LT.value;
      }
      ;
      throw new Error("Failed pattern match at Data.CodePoint.Unicode.Internal (line 5598, column 1 - line 5598, column 45): " + [v.constructor.name, v1.constructor.name]);
    };
  };
  var getRule = function(blocks) {
    return function(unichar) {
      return function(size5) {
        var key = {
          start: unichar,
          length: 1,
          convRule: nullrule
        };
        var maybeCharBlock = bsearch(key)(blocks)(size5)(blkCmp);
        if (maybeCharBlock instanceof Nothing) {
          return Nothing.value;
        }
        ;
        if (maybeCharBlock instanceof Just) {
          return new Just(maybeCharBlock.value0.convRule);
        }
        ;
        throw new Error("Failed pattern match at Data.CodePoint.Unicode.Internal (line 5612, column 5 - line 5614, column 60): " + [maybeCharBlock.constructor.name]);
      };
    };
  };
  var allchars = [{
    start: 0,
    length: 32,
    convRule: rule0
  }, {
    start: 32,
    length: 1,
    convRule: rule1
  }, {
    start: 33,
    length: 3,
    convRule: rule2
  }, {
    start: 36,
    length: 1,
    convRule: rule3
  }, {
    start: 37,
    length: 3,
    convRule: rule2
  }, {
    start: 40,
    length: 1,
    convRule: rule4
  }, {
    start: 41,
    length: 1,
    convRule: rule5
  }, {
    start: 42,
    length: 1,
    convRule: rule2
  }, {
    start: 43,
    length: 1,
    convRule: rule6
  }, {
    start: 44,
    length: 1,
    convRule: rule2
  }, {
    start: 45,
    length: 1,
    convRule: rule7
  }, {
    start: 46,
    length: 2,
    convRule: rule2
  }, {
    start: 48,
    length: 10,
    convRule: rule8
  }, {
    start: 58,
    length: 2,
    convRule: rule2
  }, {
    start: 60,
    length: 3,
    convRule: rule6
  }, {
    start: 63,
    length: 2,
    convRule: rule2
  }, {
    start: 65,
    length: 26,
    convRule: rule9
  }, {
    start: 91,
    length: 1,
    convRule: rule4
  }, {
    start: 92,
    length: 1,
    convRule: rule2
  }, {
    start: 93,
    length: 1,
    convRule: rule5
  }, {
    start: 94,
    length: 1,
    convRule: rule10
  }, {
    start: 95,
    length: 1,
    convRule: rule11
  }, {
    start: 96,
    length: 1,
    convRule: rule10
  }, {
    start: 97,
    length: 26,
    convRule: rule12
  }, {
    start: 123,
    length: 1,
    convRule: rule4
  }, {
    start: 124,
    length: 1,
    convRule: rule6
  }, {
    start: 125,
    length: 1,
    convRule: rule5
  }, {
    start: 126,
    length: 1,
    convRule: rule6
  }, {
    start: 127,
    length: 33,
    convRule: rule0
  }, {
    start: 160,
    length: 1,
    convRule: rule1
  }, {
    start: 161,
    length: 1,
    convRule: rule2
  }, {
    start: 162,
    length: 4,
    convRule: rule3
  }, {
    start: 166,
    length: 1,
    convRule: rule13
  }, {
    start: 167,
    length: 1,
    convRule: rule2
  }, {
    start: 168,
    length: 1,
    convRule: rule10
  }, {
    start: 169,
    length: 1,
    convRule: rule13
  }, {
    start: 170,
    length: 1,
    convRule: rule14
  }, {
    start: 171,
    length: 1,
    convRule: rule15
  }, {
    start: 172,
    length: 1,
    convRule: rule6
  }, {
    start: 173,
    length: 1,
    convRule: rule16
  }, {
    start: 174,
    length: 1,
    convRule: rule13
  }, {
    start: 175,
    length: 1,
    convRule: rule10
  }, {
    start: 176,
    length: 1,
    convRule: rule13
  }, {
    start: 177,
    length: 1,
    convRule: rule6
  }, {
    start: 178,
    length: 2,
    convRule: rule17
  }, {
    start: 180,
    length: 1,
    convRule: rule10
  }, {
    start: 181,
    length: 1,
    convRule: rule18
  }, {
    start: 182,
    length: 2,
    convRule: rule2
  }, {
    start: 184,
    length: 1,
    convRule: rule10
  }, {
    start: 185,
    length: 1,
    convRule: rule17
  }, {
    start: 186,
    length: 1,
    convRule: rule14
  }, {
    start: 187,
    length: 1,
    convRule: rule19
  }, {
    start: 188,
    length: 3,
    convRule: rule17
  }, {
    start: 191,
    length: 1,
    convRule: rule2
  }, {
    start: 192,
    length: 23,
    convRule: rule9
  }, {
    start: 215,
    length: 1,
    convRule: rule6
  }, {
    start: 216,
    length: 7,
    convRule: rule9
  }, {
    start: 223,
    length: 1,
    convRule: rule20
  }, {
    start: 224,
    length: 23,
    convRule: rule12
  }, {
    start: 247,
    length: 1,
    convRule: rule6
  }, {
    start: 248,
    length: 7,
    convRule: rule12
  }, {
    start: 255,
    length: 1,
    convRule: rule21
  }, {
    start: 256,
    length: 1,
    convRule: rule22
  }, {
    start: 257,
    length: 1,
    convRule: rule23
  }, {
    start: 258,
    length: 1,
    convRule: rule22
  }, {
    start: 259,
    length: 1,
    convRule: rule23
  }, {
    start: 260,
    length: 1,
    convRule: rule22
  }, {
    start: 261,
    length: 1,
    convRule: rule23
  }, {
    start: 262,
    length: 1,
    convRule: rule22
  }, {
    start: 263,
    length: 1,
    convRule: rule23
  }, {
    start: 264,
    length: 1,
    convRule: rule22
  }, {
    start: 265,
    length: 1,
    convRule: rule23
  }, {
    start: 266,
    length: 1,
    convRule: rule22
  }, {
    start: 267,
    length: 1,
    convRule: rule23
  }, {
    start: 268,
    length: 1,
    convRule: rule22
  }, {
    start: 269,
    length: 1,
    convRule: rule23
  }, {
    start: 270,
    length: 1,
    convRule: rule22
  }, {
    start: 271,
    length: 1,
    convRule: rule23
  }, {
    start: 272,
    length: 1,
    convRule: rule22
  }, {
    start: 273,
    length: 1,
    convRule: rule23
  }, {
    start: 274,
    length: 1,
    convRule: rule22
  }, {
    start: 275,
    length: 1,
    convRule: rule23
  }, {
    start: 276,
    length: 1,
    convRule: rule22
  }, {
    start: 277,
    length: 1,
    convRule: rule23
  }, {
    start: 278,
    length: 1,
    convRule: rule22
  }, {
    start: 279,
    length: 1,
    convRule: rule23
  }, {
    start: 280,
    length: 1,
    convRule: rule22
  }, {
    start: 281,
    length: 1,
    convRule: rule23
  }, {
    start: 282,
    length: 1,
    convRule: rule22
  }, {
    start: 283,
    length: 1,
    convRule: rule23
  }, {
    start: 284,
    length: 1,
    convRule: rule22
  }, {
    start: 285,
    length: 1,
    convRule: rule23
  }, {
    start: 286,
    length: 1,
    convRule: rule22
  }, {
    start: 287,
    length: 1,
    convRule: rule23
  }, {
    start: 288,
    length: 1,
    convRule: rule22
  }, {
    start: 289,
    length: 1,
    convRule: rule23
  }, {
    start: 290,
    length: 1,
    convRule: rule22
  }, {
    start: 291,
    length: 1,
    convRule: rule23
  }, {
    start: 292,
    length: 1,
    convRule: rule22
  }, {
    start: 293,
    length: 1,
    convRule: rule23
  }, {
    start: 294,
    length: 1,
    convRule: rule22
  }, {
    start: 295,
    length: 1,
    convRule: rule23
  }, {
    start: 296,
    length: 1,
    convRule: rule22
  }, {
    start: 297,
    length: 1,
    convRule: rule23
  }, {
    start: 298,
    length: 1,
    convRule: rule22
  }, {
    start: 299,
    length: 1,
    convRule: rule23
  }, {
    start: 300,
    length: 1,
    convRule: rule22
  }, {
    start: 301,
    length: 1,
    convRule: rule23
  }, {
    start: 302,
    length: 1,
    convRule: rule22
  }, {
    start: 303,
    length: 1,
    convRule: rule23
  }, {
    start: 304,
    length: 1,
    convRule: rule24
  }, {
    start: 305,
    length: 1,
    convRule: rule25
  }, {
    start: 306,
    length: 1,
    convRule: rule22
  }, {
    start: 307,
    length: 1,
    convRule: rule23
  }, {
    start: 308,
    length: 1,
    convRule: rule22
  }, {
    start: 309,
    length: 1,
    convRule: rule23
  }, {
    start: 310,
    length: 1,
    convRule: rule22
  }, {
    start: 311,
    length: 1,
    convRule: rule23
  }, {
    start: 312,
    length: 1,
    convRule: rule20
  }, {
    start: 313,
    length: 1,
    convRule: rule22
  }, {
    start: 314,
    length: 1,
    convRule: rule23
  }, {
    start: 315,
    length: 1,
    convRule: rule22
  }, {
    start: 316,
    length: 1,
    convRule: rule23
  }, {
    start: 317,
    length: 1,
    convRule: rule22
  }, {
    start: 318,
    length: 1,
    convRule: rule23
  }, {
    start: 319,
    length: 1,
    convRule: rule22
  }, {
    start: 320,
    length: 1,
    convRule: rule23
  }, {
    start: 321,
    length: 1,
    convRule: rule22
  }, {
    start: 322,
    length: 1,
    convRule: rule23
  }, {
    start: 323,
    length: 1,
    convRule: rule22
  }, {
    start: 324,
    length: 1,
    convRule: rule23
  }, {
    start: 325,
    length: 1,
    convRule: rule22
  }, {
    start: 326,
    length: 1,
    convRule: rule23
  }, {
    start: 327,
    length: 1,
    convRule: rule22
  }, {
    start: 328,
    length: 1,
    convRule: rule23
  }, {
    start: 329,
    length: 1,
    convRule: rule20
  }, {
    start: 330,
    length: 1,
    convRule: rule22
  }, {
    start: 331,
    length: 1,
    convRule: rule23
  }, {
    start: 332,
    length: 1,
    convRule: rule22
  }, {
    start: 333,
    length: 1,
    convRule: rule23
  }, {
    start: 334,
    length: 1,
    convRule: rule22
  }, {
    start: 335,
    length: 1,
    convRule: rule23
  }, {
    start: 336,
    length: 1,
    convRule: rule22
  }, {
    start: 337,
    length: 1,
    convRule: rule23
  }, {
    start: 338,
    length: 1,
    convRule: rule22
  }, {
    start: 339,
    length: 1,
    convRule: rule23
  }, {
    start: 340,
    length: 1,
    convRule: rule22
  }, {
    start: 341,
    length: 1,
    convRule: rule23
  }, {
    start: 342,
    length: 1,
    convRule: rule22
  }, {
    start: 343,
    length: 1,
    convRule: rule23
  }, {
    start: 344,
    length: 1,
    convRule: rule22
  }, {
    start: 345,
    length: 1,
    convRule: rule23
  }, {
    start: 346,
    length: 1,
    convRule: rule22
  }, {
    start: 347,
    length: 1,
    convRule: rule23
  }, {
    start: 348,
    length: 1,
    convRule: rule22
  }, {
    start: 349,
    length: 1,
    convRule: rule23
  }, {
    start: 350,
    length: 1,
    convRule: rule22
  }, {
    start: 351,
    length: 1,
    convRule: rule23
  }, {
    start: 352,
    length: 1,
    convRule: rule22
  }, {
    start: 353,
    length: 1,
    convRule: rule23
  }, {
    start: 354,
    length: 1,
    convRule: rule22
  }, {
    start: 355,
    length: 1,
    convRule: rule23
  }, {
    start: 356,
    length: 1,
    convRule: rule22
  }, {
    start: 357,
    length: 1,
    convRule: rule23
  }, {
    start: 358,
    length: 1,
    convRule: rule22
  }, {
    start: 359,
    length: 1,
    convRule: rule23
  }, {
    start: 360,
    length: 1,
    convRule: rule22
  }, {
    start: 361,
    length: 1,
    convRule: rule23
  }, {
    start: 362,
    length: 1,
    convRule: rule22
  }, {
    start: 363,
    length: 1,
    convRule: rule23
  }, {
    start: 364,
    length: 1,
    convRule: rule22
  }, {
    start: 365,
    length: 1,
    convRule: rule23
  }, {
    start: 366,
    length: 1,
    convRule: rule22
  }, {
    start: 367,
    length: 1,
    convRule: rule23
  }, {
    start: 368,
    length: 1,
    convRule: rule22
  }, {
    start: 369,
    length: 1,
    convRule: rule23
  }, {
    start: 370,
    length: 1,
    convRule: rule22
  }, {
    start: 371,
    length: 1,
    convRule: rule23
  }, {
    start: 372,
    length: 1,
    convRule: rule22
  }, {
    start: 373,
    length: 1,
    convRule: rule23
  }, {
    start: 374,
    length: 1,
    convRule: rule22
  }, {
    start: 375,
    length: 1,
    convRule: rule23
  }, {
    start: 376,
    length: 1,
    convRule: rule26
  }, {
    start: 377,
    length: 1,
    convRule: rule22
  }, {
    start: 378,
    length: 1,
    convRule: rule23
  }, {
    start: 379,
    length: 1,
    convRule: rule22
  }, {
    start: 380,
    length: 1,
    convRule: rule23
  }, {
    start: 381,
    length: 1,
    convRule: rule22
  }, {
    start: 382,
    length: 1,
    convRule: rule23
  }, {
    start: 383,
    length: 1,
    convRule: rule27
  }, {
    start: 384,
    length: 1,
    convRule: rule28
  }, {
    start: 385,
    length: 1,
    convRule: rule29
  }, {
    start: 386,
    length: 1,
    convRule: rule22
  }, {
    start: 387,
    length: 1,
    convRule: rule23
  }, {
    start: 388,
    length: 1,
    convRule: rule22
  }, {
    start: 389,
    length: 1,
    convRule: rule23
  }, {
    start: 390,
    length: 1,
    convRule: rule30
  }, {
    start: 391,
    length: 1,
    convRule: rule22
  }, {
    start: 392,
    length: 1,
    convRule: rule23
  }, {
    start: 393,
    length: 2,
    convRule: rule31
  }, {
    start: 395,
    length: 1,
    convRule: rule22
  }, {
    start: 396,
    length: 1,
    convRule: rule23
  }, {
    start: 397,
    length: 1,
    convRule: rule20
  }, {
    start: 398,
    length: 1,
    convRule: rule32
  }, {
    start: 399,
    length: 1,
    convRule: rule33
  }, {
    start: 400,
    length: 1,
    convRule: rule34
  }, {
    start: 401,
    length: 1,
    convRule: rule22
  }, {
    start: 402,
    length: 1,
    convRule: rule23
  }, {
    start: 403,
    length: 1,
    convRule: rule31
  }, {
    start: 404,
    length: 1,
    convRule: rule35
  }, {
    start: 405,
    length: 1,
    convRule: rule36
  }, {
    start: 406,
    length: 1,
    convRule: rule37
  }, {
    start: 407,
    length: 1,
    convRule: rule38
  }, {
    start: 408,
    length: 1,
    convRule: rule22
  }, {
    start: 409,
    length: 1,
    convRule: rule23
  }, {
    start: 410,
    length: 1,
    convRule: rule39
  }, {
    start: 411,
    length: 1,
    convRule: rule20
  }, {
    start: 412,
    length: 1,
    convRule: rule37
  }, {
    start: 413,
    length: 1,
    convRule: rule40
  }, {
    start: 414,
    length: 1,
    convRule: rule41
  }, {
    start: 415,
    length: 1,
    convRule: rule42
  }, {
    start: 416,
    length: 1,
    convRule: rule22
  }, {
    start: 417,
    length: 1,
    convRule: rule23
  }, {
    start: 418,
    length: 1,
    convRule: rule22
  }, {
    start: 419,
    length: 1,
    convRule: rule23
  }, {
    start: 420,
    length: 1,
    convRule: rule22
  }, {
    start: 421,
    length: 1,
    convRule: rule23
  }, {
    start: 422,
    length: 1,
    convRule: rule43
  }, {
    start: 423,
    length: 1,
    convRule: rule22
  }, {
    start: 424,
    length: 1,
    convRule: rule23
  }, {
    start: 425,
    length: 1,
    convRule: rule43
  }, {
    start: 426,
    length: 2,
    convRule: rule20
  }, {
    start: 428,
    length: 1,
    convRule: rule22
  }, {
    start: 429,
    length: 1,
    convRule: rule23
  }, {
    start: 430,
    length: 1,
    convRule: rule43
  }, {
    start: 431,
    length: 1,
    convRule: rule22
  }, {
    start: 432,
    length: 1,
    convRule: rule23
  }, {
    start: 433,
    length: 2,
    convRule: rule44
  }, {
    start: 435,
    length: 1,
    convRule: rule22
  }, {
    start: 436,
    length: 1,
    convRule: rule23
  }, {
    start: 437,
    length: 1,
    convRule: rule22
  }, {
    start: 438,
    length: 1,
    convRule: rule23
  }, {
    start: 439,
    length: 1,
    convRule: rule45
  }, {
    start: 440,
    length: 1,
    convRule: rule22
  }, {
    start: 441,
    length: 1,
    convRule: rule23
  }, {
    start: 442,
    length: 1,
    convRule: rule20
  }, {
    start: 443,
    length: 1,
    convRule: rule14
  }, {
    start: 444,
    length: 1,
    convRule: rule22
  }, {
    start: 445,
    length: 1,
    convRule: rule23
  }, {
    start: 446,
    length: 1,
    convRule: rule20
  }, {
    start: 447,
    length: 1,
    convRule: rule46
  }, {
    start: 448,
    length: 4,
    convRule: rule14
  }, {
    start: 452,
    length: 1,
    convRule: rule47
  }, {
    start: 453,
    length: 1,
    convRule: rule48
  }, {
    start: 454,
    length: 1,
    convRule: rule49
  }, {
    start: 455,
    length: 1,
    convRule: rule47
  }, {
    start: 456,
    length: 1,
    convRule: rule48
  }, {
    start: 457,
    length: 1,
    convRule: rule49
  }, {
    start: 458,
    length: 1,
    convRule: rule47
  }, {
    start: 459,
    length: 1,
    convRule: rule48
  }, {
    start: 460,
    length: 1,
    convRule: rule49
  }, {
    start: 461,
    length: 1,
    convRule: rule22
  }, {
    start: 462,
    length: 1,
    convRule: rule23
  }, {
    start: 463,
    length: 1,
    convRule: rule22
  }, {
    start: 464,
    length: 1,
    convRule: rule23
  }, {
    start: 465,
    length: 1,
    convRule: rule22
  }, {
    start: 466,
    length: 1,
    convRule: rule23
  }, {
    start: 467,
    length: 1,
    convRule: rule22
  }, {
    start: 468,
    length: 1,
    convRule: rule23
  }, {
    start: 469,
    length: 1,
    convRule: rule22
  }, {
    start: 470,
    length: 1,
    convRule: rule23
  }, {
    start: 471,
    length: 1,
    convRule: rule22
  }, {
    start: 472,
    length: 1,
    convRule: rule23
  }, {
    start: 473,
    length: 1,
    convRule: rule22
  }, {
    start: 474,
    length: 1,
    convRule: rule23
  }, {
    start: 475,
    length: 1,
    convRule: rule22
  }, {
    start: 476,
    length: 1,
    convRule: rule23
  }, {
    start: 477,
    length: 1,
    convRule: rule50
  }, {
    start: 478,
    length: 1,
    convRule: rule22
  }, {
    start: 479,
    length: 1,
    convRule: rule23
  }, {
    start: 480,
    length: 1,
    convRule: rule22
  }, {
    start: 481,
    length: 1,
    convRule: rule23
  }, {
    start: 482,
    length: 1,
    convRule: rule22
  }, {
    start: 483,
    length: 1,
    convRule: rule23
  }, {
    start: 484,
    length: 1,
    convRule: rule22
  }, {
    start: 485,
    length: 1,
    convRule: rule23
  }, {
    start: 486,
    length: 1,
    convRule: rule22
  }, {
    start: 487,
    length: 1,
    convRule: rule23
  }, {
    start: 488,
    length: 1,
    convRule: rule22
  }, {
    start: 489,
    length: 1,
    convRule: rule23
  }, {
    start: 490,
    length: 1,
    convRule: rule22
  }, {
    start: 491,
    length: 1,
    convRule: rule23
  }, {
    start: 492,
    length: 1,
    convRule: rule22
  }, {
    start: 493,
    length: 1,
    convRule: rule23
  }, {
    start: 494,
    length: 1,
    convRule: rule22
  }, {
    start: 495,
    length: 1,
    convRule: rule23
  }, {
    start: 496,
    length: 1,
    convRule: rule20
  }, {
    start: 497,
    length: 1,
    convRule: rule47
  }, {
    start: 498,
    length: 1,
    convRule: rule48
  }, {
    start: 499,
    length: 1,
    convRule: rule49
  }, {
    start: 500,
    length: 1,
    convRule: rule22
  }, {
    start: 501,
    length: 1,
    convRule: rule23
  }, {
    start: 502,
    length: 1,
    convRule: rule51
  }, {
    start: 503,
    length: 1,
    convRule: rule52
  }, {
    start: 504,
    length: 1,
    convRule: rule22
  }, {
    start: 505,
    length: 1,
    convRule: rule23
  }, {
    start: 506,
    length: 1,
    convRule: rule22
  }, {
    start: 507,
    length: 1,
    convRule: rule23
  }, {
    start: 508,
    length: 1,
    convRule: rule22
  }, {
    start: 509,
    length: 1,
    convRule: rule23
  }, {
    start: 510,
    length: 1,
    convRule: rule22
  }, {
    start: 511,
    length: 1,
    convRule: rule23
  }, {
    start: 512,
    length: 1,
    convRule: rule22
  }, {
    start: 513,
    length: 1,
    convRule: rule23
  }, {
    start: 514,
    length: 1,
    convRule: rule22
  }, {
    start: 515,
    length: 1,
    convRule: rule23
  }, {
    start: 516,
    length: 1,
    convRule: rule22
  }, {
    start: 517,
    length: 1,
    convRule: rule23
  }, {
    start: 518,
    length: 1,
    convRule: rule22
  }, {
    start: 519,
    length: 1,
    convRule: rule23
  }, {
    start: 520,
    length: 1,
    convRule: rule22
  }, {
    start: 521,
    length: 1,
    convRule: rule23
  }, {
    start: 522,
    length: 1,
    convRule: rule22
  }, {
    start: 523,
    length: 1,
    convRule: rule23
  }, {
    start: 524,
    length: 1,
    convRule: rule22
  }, {
    start: 525,
    length: 1,
    convRule: rule23
  }, {
    start: 526,
    length: 1,
    convRule: rule22
  }, {
    start: 527,
    length: 1,
    convRule: rule23
  }, {
    start: 528,
    length: 1,
    convRule: rule22
  }, {
    start: 529,
    length: 1,
    convRule: rule23
  }, {
    start: 530,
    length: 1,
    convRule: rule22
  }, {
    start: 531,
    length: 1,
    convRule: rule23
  }, {
    start: 532,
    length: 1,
    convRule: rule22
  }, {
    start: 533,
    length: 1,
    convRule: rule23
  }, {
    start: 534,
    length: 1,
    convRule: rule22
  }, {
    start: 535,
    length: 1,
    convRule: rule23
  }, {
    start: 536,
    length: 1,
    convRule: rule22
  }, {
    start: 537,
    length: 1,
    convRule: rule23
  }, {
    start: 538,
    length: 1,
    convRule: rule22
  }, {
    start: 539,
    length: 1,
    convRule: rule23
  }, {
    start: 540,
    length: 1,
    convRule: rule22
  }, {
    start: 541,
    length: 1,
    convRule: rule23
  }, {
    start: 542,
    length: 1,
    convRule: rule22
  }, {
    start: 543,
    length: 1,
    convRule: rule23
  }, {
    start: 544,
    length: 1,
    convRule: rule53
  }, {
    start: 545,
    length: 1,
    convRule: rule20
  }, {
    start: 546,
    length: 1,
    convRule: rule22
  }, {
    start: 547,
    length: 1,
    convRule: rule23
  }, {
    start: 548,
    length: 1,
    convRule: rule22
  }, {
    start: 549,
    length: 1,
    convRule: rule23
  }, {
    start: 550,
    length: 1,
    convRule: rule22
  }, {
    start: 551,
    length: 1,
    convRule: rule23
  }, {
    start: 552,
    length: 1,
    convRule: rule22
  }, {
    start: 553,
    length: 1,
    convRule: rule23
  }, {
    start: 554,
    length: 1,
    convRule: rule22
  }, {
    start: 555,
    length: 1,
    convRule: rule23
  }, {
    start: 556,
    length: 1,
    convRule: rule22
  }, {
    start: 557,
    length: 1,
    convRule: rule23
  }, {
    start: 558,
    length: 1,
    convRule: rule22
  }, {
    start: 559,
    length: 1,
    convRule: rule23
  }, {
    start: 560,
    length: 1,
    convRule: rule22
  }, {
    start: 561,
    length: 1,
    convRule: rule23
  }, {
    start: 562,
    length: 1,
    convRule: rule22
  }, {
    start: 563,
    length: 1,
    convRule: rule23
  }, {
    start: 564,
    length: 6,
    convRule: rule20
  }, {
    start: 570,
    length: 1,
    convRule: rule54
  }, {
    start: 571,
    length: 1,
    convRule: rule22
  }, {
    start: 572,
    length: 1,
    convRule: rule23
  }, {
    start: 573,
    length: 1,
    convRule: rule55
  }, {
    start: 574,
    length: 1,
    convRule: rule56
  }, {
    start: 575,
    length: 2,
    convRule: rule57
  }, {
    start: 577,
    length: 1,
    convRule: rule22
  }, {
    start: 578,
    length: 1,
    convRule: rule23
  }, {
    start: 579,
    length: 1,
    convRule: rule58
  }, {
    start: 580,
    length: 1,
    convRule: rule59
  }, {
    start: 581,
    length: 1,
    convRule: rule60
  }, {
    start: 582,
    length: 1,
    convRule: rule22
  }, {
    start: 583,
    length: 1,
    convRule: rule23
  }, {
    start: 584,
    length: 1,
    convRule: rule22
  }, {
    start: 585,
    length: 1,
    convRule: rule23
  }, {
    start: 586,
    length: 1,
    convRule: rule22
  }, {
    start: 587,
    length: 1,
    convRule: rule23
  }, {
    start: 588,
    length: 1,
    convRule: rule22
  }, {
    start: 589,
    length: 1,
    convRule: rule23
  }, {
    start: 590,
    length: 1,
    convRule: rule22
  }, {
    start: 591,
    length: 1,
    convRule: rule23
  }, {
    start: 592,
    length: 1,
    convRule: rule61
  }, {
    start: 593,
    length: 1,
    convRule: rule62
  }, {
    start: 594,
    length: 1,
    convRule: rule63
  }, {
    start: 595,
    length: 1,
    convRule: rule64
  }, {
    start: 596,
    length: 1,
    convRule: rule65
  }, {
    start: 597,
    length: 1,
    convRule: rule20
  }, {
    start: 598,
    length: 2,
    convRule: rule66
  }, {
    start: 600,
    length: 1,
    convRule: rule20
  }, {
    start: 601,
    length: 1,
    convRule: rule67
  }, {
    start: 602,
    length: 1,
    convRule: rule20
  }, {
    start: 603,
    length: 1,
    convRule: rule68
  }, {
    start: 604,
    length: 1,
    convRule: rule69
  }, {
    start: 605,
    length: 3,
    convRule: rule20
  }, {
    start: 608,
    length: 1,
    convRule: rule66
  }, {
    start: 609,
    length: 1,
    convRule: rule70
  }, {
    start: 610,
    length: 1,
    convRule: rule20
  }, {
    start: 611,
    length: 1,
    convRule: rule71
  }, {
    start: 612,
    length: 1,
    convRule: rule20
  }, {
    start: 613,
    length: 1,
    convRule: rule72
  }, {
    start: 614,
    length: 1,
    convRule: rule73
  }, {
    start: 615,
    length: 1,
    convRule: rule20
  }, {
    start: 616,
    length: 1,
    convRule: rule74
  }, {
    start: 617,
    length: 1,
    convRule: rule75
  }, {
    start: 618,
    length: 1,
    convRule: rule73
  }, {
    start: 619,
    length: 1,
    convRule: rule76
  }, {
    start: 620,
    length: 1,
    convRule: rule77
  }, {
    start: 621,
    length: 2,
    convRule: rule20
  }, {
    start: 623,
    length: 1,
    convRule: rule75
  }, {
    start: 624,
    length: 1,
    convRule: rule20
  }, {
    start: 625,
    length: 1,
    convRule: rule78
  }, {
    start: 626,
    length: 1,
    convRule: rule79
  }, {
    start: 627,
    length: 2,
    convRule: rule20
  }, {
    start: 629,
    length: 1,
    convRule: rule80
  }, {
    start: 630,
    length: 7,
    convRule: rule20
  }, {
    start: 637,
    length: 1,
    convRule: rule81
  }, {
    start: 638,
    length: 2,
    convRule: rule20
  }, {
    start: 640,
    length: 1,
    convRule: rule82
  }, {
    start: 641,
    length: 1,
    convRule: rule20
  }, {
    start: 642,
    length: 1,
    convRule: rule83
  }, {
    start: 643,
    length: 1,
    convRule: rule82
  }, {
    start: 644,
    length: 3,
    convRule: rule20
  }, {
    start: 647,
    length: 1,
    convRule: rule84
  }, {
    start: 648,
    length: 1,
    convRule: rule82
  }, {
    start: 649,
    length: 1,
    convRule: rule85
  }, {
    start: 650,
    length: 2,
    convRule: rule86
  }, {
    start: 652,
    length: 1,
    convRule: rule87
  }, {
    start: 653,
    length: 5,
    convRule: rule20
  }, {
    start: 658,
    length: 1,
    convRule: rule88
  }, {
    start: 659,
    length: 1,
    convRule: rule20
  }, {
    start: 660,
    length: 1,
    convRule: rule14
  }, {
    start: 661,
    length: 8,
    convRule: rule20
  }, {
    start: 669,
    length: 1,
    convRule: rule89
  }, {
    start: 670,
    length: 1,
    convRule: rule90
  }, {
    start: 671,
    length: 17,
    convRule: rule20
  }, {
    start: 688,
    length: 18,
    convRule: rule91
  }, {
    start: 706,
    length: 4,
    convRule: rule10
  }, {
    start: 710,
    length: 12,
    convRule: rule91
  }, {
    start: 722,
    length: 14,
    convRule: rule10
  }, {
    start: 736,
    length: 5,
    convRule: rule91
  }, {
    start: 741,
    length: 7,
    convRule: rule10
  }, {
    start: 748,
    length: 1,
    convRule: rule91
  }, {
    start: 749,
    length: 1,
    convRule: rule10
  }, {
    start: 750,
    length: 1,
    convRule: rule91
  }, {
    start: 751,
    length: 17,
    convRule: rule10
  }, {
    start: 768,
    length: 69,
    convRule: rule92
  }, {
    start: 837,
    length: 1,
    convRule: rule93
  }, {
    start: 838,
    length: 42,
    convRule: rule92
  }, {
    start: 880,
    length: 1,
    convRule: rule22
  }, {
    start: 881,
    length: 1,
    convRule: rule23
  }, {
    start: 882,
    length: 1,
    convRule: rule22
  }, {
    start: 883,
    length: 1,
    convRule: rule23
  }, {
    start: 884,
    length: 1,
    convRule: rule91
  }, {
    start: 885,
    length: 1,
    convRule: rule10
  }, {
    start: 886,
    length: 1,
    convRule: rule22
  }, {
    start: 887,
    length: 1,
    convRule: rule23
  }, {
    start: 890,
    length: 1,
    convRule: rule91
  }, {
    start: 891,
    length: 3,
    convRule: rule41
  }, {
    start: 894,
    length: 1,
    convRule: rule2
  }, {
    start: 895,
    length: 1,
    convRule: rule94
  }, {
    start: 900,
    length: 2,
    convRule: rule10
  }, {
    start: 902,
    length: 1,
    convRule: rule95
  }, {
    start: 903,
    length: 1,
    convRule: rule2
  }, {
    start: 904,
    length: 3,
    convRule: rule96
  }, {
    start: 908,
    length: 1,
    convRule: rule97
  }, {
    start: 910,
    length: 2,
    convRule: rule98
  }, {
    start: 912,
    length: 1,
    convRule: rule20
  }, {
    start: 913,
    length: 17,
    convRule: rule9
  }, {
    start: 931,
    length: 9,
    convRule: rule9
  }, {
    start: 940,
    length: 1,
    convRule: rule99
  }, {
    start: 941,
    length: 3,
    convRule: rule100
  }, {
    start: 944,
    length: 1,
    convRule: rule20
  }, {
    start: 945,
    length: 17,
    convRule: rule12
  }, {
    start: 962,
    length: 1,
    convRule: rule101
  }, {
    start: 963,
    length: 9,
    convRule: rule12
  }, {
    start: 972,
    length: 1,
    convRule: rule102
  }, {
    start: 973,
    length: 2,
    convRule: rule103
  }, {
    start: 975,
    length: 1,
    convRule: rule104
  }, {
    start: 976,
    length: 1,
    convRule: rule105
  }, {
    start: 977,
    length: 1,
    convRule: rule106
  }, {
    start: 978,
    length: 3,
    convRule: rule107
  }, {
    start: 981,
    length: 1,
    convRule: rule108
  }, {
    start: 982,
    length: 1,
    convRule: rule109
  }, {
    start: 983,
    length: 1,
    convRule: rule110
  }, {
    start: 984,
    length: 1,
    convRule: rule22
  }, {
    start: 985,
    length: 1,
    convRule: rule23
  }, {
    start: 986,
    length: 1,
    convRule: rule22
  }, {
    start: 987,
    length: 1,
    convRule: rule23
  }, {
    start: 988,
    length: 1,
    convRule: rule22
  }, {
    start: 989,
    length: 1,
    convRule: rule23
  }, {
    start: 990,
    length: 1,
    convRule: rule22
  }, {
    start: 991,
    length: 1,
    convRule: rule23
  }, {
    start: 992,
    length: 1,
    convRule: rule22
  }, {
    start: 993,
    length: 1,
    convRule: rule23
  }, {
    start: 994,
    length: 1,
    convRule: rule22
  }, {
    start: 995,
    length: 1,
    convRule: rule23
  }, {
    start: 996,
    length: 1,
    convRule: rule22
  }, {
    start: 997,
    length: 1,
    convRule: rule23
  }, {
    start: 998,
    length: 1,
    convRule: rule22
  }, {
    start: 999,
    length: 1,
    convRule: rule23
  }, {
    start: 1e3,
    length: 1,
    convRule: rule22
  }, {
    start: 1001,
    length: 1,
    convRule: rule23
  }, {
    start: 1002,
    length: 1,
    convRule: rule22
  }, {
    start: 1003,
    length: 1,
    convRule: rule23
  }, {
    start: 1004,
    length: 1,
    convRule: rule22
  }, {
    start: 1005,
    length: 1,
    convRule: rule23
  }, {
    start: 1006,
    length: 1,
    convRule: rule22
  }, {
    start: 1007,
    length: 1,
    convRule: rule23
  }, {
    start: 1008,
    length: 1,
    convRule: rule111
  }, {
    start: 1009,
    length: 1,
    convRule: rule112
  }, {
    start: 1010,
    length: 1,
    convRule: rule113
  }, {
    start: 1011,
    length: 1,
    convRule: rule114
  }, {
    start: 1012,
    length: 1,
    convRule: rule115
  }, {
    start: 1013,
    length: 1,
    convRule: rule116
  }, {
    start: 1014,
    length: 1,
    convRule: rule6
  }, {
    start: 1015,
    length: 1,
    convRule: rule22
  }, {
    start: 1016,
    length: 1,
    convRule: rule23
  }, {
    start: 1017,
    length: 1,
    convRule: rule117
  }, {
    start: 1018,
    length: 1,
    convRule: rule22
  }, {
    start: 1019,
    length: 1,
    convRule: rule23
  }, {
    start: 1020,
    length: 1,
    convRule: rule20
  }, {
    start: 1021,
    length: 3,
    convRule: rule53
  }, {
    start: 1024,
    length: 16,
    convRule: rule118
  }, {
    start: 1040,
    length: 32,
    convRule: rule9
  }, {
    start: 1072,
    length: 32,
    convRule: rule12
  }, {
    start: 1104,
    length: 16,
    convRule: rule112
  }, {
    start: 1120,
    length: 1,
    convRule: rule22
  }, {
    start: 1121,
    length: 1,
    convRule: rule23
  }, {
    start: 1122,
    length: 1,
    convRule: rule22
  }, {
    start: 1123,
    length: 1,
    convRule: rule23
  }, {
    start: 1124,
    length: 1,
    convRule: rule22
  }, {
    start: 1125,
    length: 1,
    convRule: rule23
  }, {
    start: 1126,
    length: 1,
    convRule: rule22
  }, {
    start: 1127,
    length: 1,
    convRule: rule23
  }, {
    start: 1128,
    length: 1,
    convRule: rule22
  }, {
    start: 1129,
    length: 1,
    convRule: rule23
  }, {
    start: 1130,
    length: 1,
    convRule: rule22
  }, {
    start: 1131,
    length: 1,
    convRule: rule23
  }, {
    start: 1132,
    length: 1,
    convRule: rule22
  }, {
    start: 1133,
    length: 1,
    convRule: rule23
  }, {
    start: 1134,
    length: 1,
    convRule: rule22
  }, {
    start: 1135,
    length: 1,
    convRule: rule23
  }, {
    start: 1136,
    length: 1,
    convRule: rule22
  }, {
    start: 1137,
    length: 1,
    convRule: rule23
  }, {
    start: 1138,
    length: 1,
    convRule: rule22
  }, {
    start: 1139,
    length: 1,
    convRule: rule23
  }, {
    start: 1140,
    length: 1,
    convRule: rule22
  }, {
    start: 1141,
    length: 1,
    convRule: rule23
  }, {
    start: 1142,
    length: 1,
    convRule: rule22
  }, {
    start: 1143,
    length: 1,
    convRule: rule23
  }, {
    start: 1144,
    length: 1,
    convRule: rule22
  }, {
    start: 1145,
    length: 1,
    convRule: rule23
  }, {
    start: 1146,
    length: 1,
    convRule: rule22
  }, {
    start: 1147,
    length: 1,
    convRule: rule23
  }, {
    start: 1148,
    length: 1,
    convRule: rule22
  }, {
    start: 1149,
    length: 1,
    convRule: rule23
  }, {
    start: 1150,
    length: 1,
    convRule: rule22
  }, {
    start: 1151,
    length: 1,
    convRule: rule23
  }, {
    start: 1152,
    length: 1,
    convRule: rule22
  }, {
    start: 1153,
    length: 1,
    convRule: rule23
  }, {
    start: 1154,
    length: 1,
    convRule: rule13
  }, {
    start: 1155,
    length: 5,
    convRule: rule92
  }, {
    start: 1160,
    length: 2,
    convRule: rule119
  }, {
    start: 1162,
    length: 1,
    convRule: rule22
  }, {
    start: 1163,
    length: 1,
    convRule: rule23
  }, {
    start: 1164,
    length: 1,
    convRule: rule22
  }, {
    start: 1165,
    length: 1,
    convRule: rule23
  }, {
    start: 1166,
    length: 1,
    convRule: rule22
  }, {
    start: 1167,
    length: 1,
    convRule: rule23
  }, {
    start: 1168,
    length: 1,
    convRule: rule22
  }, {
    start: 1169,
    length: 1,
    convRule: rule23
  }, {
    start: 1170,
    length: 1,
    convRule: rule22
  }, {
    start: 1171,
    length: 1,
    convRule: rule23
  }, {
    start: 1172,
    length: 1,
    convRule: rule22
  }, {
    start: 1173,
    length: 1,
    convRule: rule23
  }, {
    start: 1174,
    length: 1,
    convRule: rule22
  }, {
    start: 1175,
    length: 1,
    convRule: rule23
  }, {
    start: 1176,
    length: 1,
    convRule: rule22
  }, {
    start: 1177,
    length: 1,
    convRule: rule23
  }, {
    start: 1178,
    length: 1,
    convRule: rule22
  }, {
    start: 1179,
    length: 1,
    convRule: rule23
  }, {
    start: 1180,
    length: 1,
    convRule: rule22
  }, {
    start: 1181,
    length: 1,
    convRule: rule23
  }, {
    start: 1182,
    length: 1,
    convRule: rule22
  }, {
    start: 1183,
    length: 1,
    convRule: rule23
  }, {
    start: 1184,
    length: 1,
    convRule: rule22
  }, {
    start: 1185,
    length: 1,
    convRule: rule23
  }, {
    start: 1186,
    length: 1,
    convRule: rule22
  }, {
    start: 1187,
    length: 1,
    convRule: rule23
  }, {
    start: 1188,
    length: 1,
    convRule: rule22
  }, {
    start: 1189,
    length: 1,
    convRule: rule23
  }, {
    start: 1190,
    length: 1,
    convRule: rule22
  }, {
    start: 1191,
    length: 1,
    convRule: rule23
  }, {
    start: 1192,
    length: 1,
    convRule: rule22
  }, {
    start: 1193,
    length: 1,
    convRule: rule23
  }, {
    start: 1194,
    length: 1,
    convRule: rule22
  }, {
    start: 1195,
    length: 1,
    convRule: rule23
  }, {
    start: 1196,
    length: 1,
    convRule: rule22
  }, {
    start: 1197,
    length: 1,
    convRule: rule23
  }, {
    start: 1198,
    length: 1,
    convRule: rule22
  }, {
    start: 1199,
    length: 1,
    convRule: rule23
  }, {
    start: 1200,
    length: 1,
    convRule: rule22
  }, {
    start: 1201,
    length: 1,
    convRule: rule23
  }, {
    start: 1202,
    length: 1,
    convRule: rule22
  }, {
    start: 1203,
    length: 1,
    convRule: rule23
  }, {
    start: 1204,
    length: 1,
    convRule: rule22
  }, {
    start: 1205,
    length: 1,
    convRule: rule23
  }, {
    start: 1206,
    length: 1,
    convRule: rule22
  }, {
    start: 1207,
    length: 1,
    convRule: rule23
  }, {
    start: 1208,
    length: 1,
    convRule: rule22
  }, {
    start: 1209,
    length: 1,
    convRule: rule23
  }, {
    start: 1210,
    length: 1,
    convRule: rule22
  }, {
    start: 1211,
    length: 1,
    convRule: rule23
  }, {
    start: 1212,
    length: 1,
    convRule: rule22
  }, {
    start: 1213,
    length: 1,
    convRule: rule23
  }, {
    start: 1214,
    length: 1,
    convRule: rule22
  }, {
    start: 1215,
    length: 1,
    convRule: rule23
  }, {
    start: 1216,
    length: 1,
    convRule: rule120
  }, {
    start: 1217,
    length: 1,
    convRule: rule22
  }, {
    start: 1218,
    length: 1,
    convRule: rule23
  }, {
    start: 1219,
    length: 1,
    convRule: rule22
  }, {
    start: 1220,
    length: 1,
    convRule: rule23
  }, {
    start: 1221,
    length: 1,
    convRule: rule22
  }, {
    start: 1222,
    length: 1,
    convRule: rule23
  }, {
    start: 1223,
    length: 1,
    convRule: rule22
  }, {
    start: 1224,
    length: 1,
    convRule: rule23
  }, {
    start: 1225,
    length: 1,
    convRule: rule22
  }, {
    start: 1226,
    length: 1,
    convRule: rule23
  }, {
    start: 1227,
    length: 1,
    convRule: rule22
  }, {
    start: 1228,
    length: 1,
    convRule: rule23
  }, {
    start: 1229,
    length: 1,
    convRule: rule22
  }, {
    start: 1230,
    length: 1,
    convRule: rule23
  }, {
    start: 1231,
    length: 1,
    convRule: rule121
  }, {
    start: 1232,
    length: 1,
    convRule: rule22
  }, {
    start: 1233,
    length: 1,
    convRule: rule23
  }, {
    start: 1234,
    length: 1,
    convRule: rule22
  }, {
    start: 1235,
    length: 1,
    convRule: rule23
  }, {
    start: 1236,
    length: 1,
    convRule: rule22
  }, {
    start: 1237,
    length: 1,
    convRule: rule23
  }, {
    start: 1238,
    length: 1,
    convRule: rule22
  }, {
    start: 1239,
    length: 1,
    convRule: rule23
  }, {
    start: 1240,
    length: 1,
    convRule: rule22
  }, {
    start: 1241,
    length: 1,
    convRule: rule23
  }, {
    start: 1242,
    length: 1,
    convRule: rule22
  }, {
    start: 1243,
    length: 1,
    convRule: rule23
  }, {
    start: 1244,
    length: 1,
    convRule: rule22
  }, {
    start: 1245,
    length: 1,
    convRule: rule23
  }, {
    start: 1246,
    length: 1,
    convRule: rule22
  }, {
    start: 1247,
    length: 1,
    convRule: rule23
  }, {
    start: 1248,
    length: 1,
    convRule: rule22
  }, {
    start: 1249,
    length: 1,
    convRule: rule23
  }, {
    start: 1250,
    length: 1,
    convRule: rule22
  }, {
    start: 1251,
    length: 1,
    convRule: rule23
  }, {
    start: 1252,
    length: 1,
    convRule: rule22
  }, {
    start: 1253,
    length: 1,
    convRule: rule23
  }, {
    start: 1254,
    length: 1,
    convRule: rule22
  }, {
    start: 1255,
    length: 1,
    convRule: rule23
  }, {
    start: 1256,
    length: 1,
    convRule: rule22
  }, {
    start: 1257,
    length: 1,
    convRule: rule23
  }, {
    start: 1258,
    length: 1,
    convRule: rule22
  }, {
    start: 1259,
    length: 1,
    convRule: rule23
  }, {
    start: 1260,
    length: 1,
    convRule: rule22
  }, {
    start: 1261,
    length: 1,
    convRule: rule23
  }, {
    start: 1262,
    length: 1,
    convRule: rule22
  }, {
    start: 1263,
    length: 1,
    convRule: rule23
  }, {
    start: 1264,
    length: 1,
    convRule: rule22
  }, {
    start: 1265,
    length: 1,
    convRule: rule23
  }, {
    start: 1266,
    length: 1,
    convRule: rule22
  }, {
    start: 1267,
    length: 1,
    convRule: rule23
  }, {
    start: 1268,
    length: 1,
    convRule: rule22
  }, {
    start: 1269,
    length: 1,
    convRule: rule23
  }, {
    start: 1270,
    length: 1,
    convRule: rule22
  }, {
    start: 1271,
    length: 1,
    convRule: rule23
  }, {
    start: 1272,
    length: 1,
    convRule: rule22
  }, {
    start: 1273,
    length: 1,
    convRule: rule23
  }, {
    start: 1274,
    length: 1,
    convRule: rule22
  }, {
    start: 1275,
    length: 1,
    convRule: rule23
  }, {
    start: 1276,
    length: 1,
    convRule: rule22
  }, {
    start: 1277,
    length: 1,
    convRule: rule23
  }, {
    start: 1278,
    length: 1,
    convRule: rule22
  }, {
    start: 1279,
    length: 1,
    convRule: rule23
  }, {
    start: 1280,
    length: 1,
    convRule: rule22
  }, {
    start: 1281,
    length: 1,
    convRule: rule23
  }, {
    start: 1282,
    length: 1,
    convRule: rule22
  }, {
    start: 1283,
    length: 1,
    convRule: rule23
  }, {
    start: 1284,
    length: 1,
    convRule: rule22
  }, {
    start: 1285,
    length: 1,
    convRule: rule23
  }, {
    start: 1286,
    length: 1,
    convRule: rule22
  }, {
    start: 1287,
    length: 1,
    convRule: rule23
  }, {
    start: 1288,
    length: 1,
    convRule: rule22
  }, {
    start: 1289,
    length: 1,
    convRule: rule23
  }, {
    start: 1290,
    length: 1,
    convRule: rule22
  }, {
    start: 1291,
    length: 1,
    convRule: rule23
  }, {
    start: 1292,
    length: 1,
    convRule: rule22
  }, {
    start: 1293,
    length: 1,
    convRule: rule23
  }, {
    start: 1294,
    length: 1,
    convRule: rule22
  }, {
    start: 1295,
    length: 1,
    convRule: rule23
  }, {
    start: 1296,
    length: 1,
    convRule: rule22
  }, {
    start: 1297,
    length: 1,
    convRule: rule23
  }, {
    start: 1298,
    length: 1,
    convRule: rule22
  }, {
    start: 1299,
    length: 1,
    convRule: rule23
  }, {
    start: 1300,
    length: 1,
    convRule: rule22
  }, {
    start: 1301,
    length: 1,
    convRule: rule23
  }, {
    start: 1302,
    length: 1,
    convRule: rule22
  }, {
    start: 1303,
    length: 1,
    convRule: rule23
  }, {
    start: 1304,
    length: 1,
    convRule: rule22
  }, {
    start: 1305,
    length: 1,
    convRule: rule23
  }, {
    start: 1306,
    length: 1,
    convRule: rule22
  }, {
    start: 1307,
    length: 1,
    convRule: rule23
  }, {
    start: 1308,
    length: 1,
    convRule: rule22
  }, {
    start: 1309,
    length: 1,
    convRule: rule23
  }, {
    start: 1310,
    length: 1,
    convRule: rule22
  }, {
    start: 1311,
    length: 1,
    convRule: rule23
  }, {
    start: 1312,
    length: 1,
    convRule: rule22
  }, {
    start: 1313,
    length: 1,
    convRule: rule23
  }, {
    start: 1314,
    length: 1,
    convRule: rule22
  }, {
    start: 1315,
    length: 1,
    convRule: rule23
  }, {
    start: 1316,
    length: 1,
    convRule: rule22
  }, {
    start: 1317,
    length: 1,
    convRule: rule23
  }, {
    start: 1318,
    length: 1,
    convRule: rule22
  }, {
    start: 1319,
    length: 1,
    convRule: rule23
  }, {
    start: 1320,
    length: 1,
    convRule: rule22
  }, {
    start: 1321,
    length: 1,
    convRule: rule23
  }, {
    start: 1322,
    length: 1,
    convRule: rule22
  }, {
    start: 1323,
    length: 1,
    convRule: rule23
  }, {
    start: 1324,
    length: 1,
    convRule: rule22
  }, {
    start: 1325,
    length: 1,
    convRule: rule23
  }, {
    start: 1326,
    length: 1,
    convRule: rule22
  }, {
    start: 1327,
    length: 1,
    convRule: rule23
  }, {
    start: 1329,
    length: 38,
    convRule: rule122
  }, {
    start: 1369,
    length: 1,
    convRule: rule91
  }, {
    start: 1370,
    length: 6,
    convRule: rule2
  }, {
    start: 1376,
    length: 1,
    convRule: rule20
  }, {
    start: 1377,
    length: 38,
    convRule: rule123
  }, {
    start: 1415,
    length: 2,
    convRule: rule20
  }, {
    start: 1417,
    length: 1,
    convRule: rule2
  }, {
    start: 1418,
    length: 1,
    convRule: rule7
  }, {
    start: 1421,
    length: 2,
    convRule: rule13
  }, {
    start: 1423,
    length: 1,
    convRule: rule3
  }, {
    start: 1425,
    length: 45,
    convRule: rule92
  }, {
    start: 1470,
    length: 1,
    convRule: rule7
  }, {
    start: 1471,
    length: 1,
    convRule: rule92
  }, {
    start: 1472,
    length: 1,
    convRule: rule2
  }, {
    start: 1473,
    length: 2,
    convRule: rule92
  }, {
    start: 1475,
    length: 1,
    convRule: rule2
  }, {
    start: 1476,
    length: 2,
    convRule: rule92
  }, {
    start: 1478,
    length: 1,
    convRule: rule2
  }, {
    start: 1479,
    length: 1,
    convRule: rule92
  }, {
    start: 1488,
    length: 27,
    convRule: rule14
  }, {
    start: 1519,
    length: 4,
    convRule: rule14
  }, {
    start: 1523,
    length: 2,
    convRule: rule2
  }, {
    start: 1536,
    length: 6,
    convRule: rule16
  }, {
    start: 1542,
    length: 3,
    convRule: rule6
  }, {
    start: 1545,
    length: 2,
    convRule: rule2
  }, {
    start: 1547,
    length: 1,
    convRule: rule3
  }, {
    start: 1548,
    length: 2,
    convRule: rule2
  }, {
    start: 1550,
    length: 2,
    convRule: rule13
  }, {
    start: 1552,
    length: 11,
    convRule: rule92
  }, {
    start: 1563,
    length: 1,
    convRule: rule2
  }, {
    start: 1564,
    length: 1,
    convRule: rule16
  }, {
    start: 1566,
    length: 2,
    convRule: rule2
  }, {
    start: 1568,
    length: 32,
    convRule: rule14
  }, {
    start: 1600,
    length: 1,
    convRule: rule91
  }, {
    start: 1601,
    length: 10,
    convRule: rule14
  }, {
    start: 1611,
    length: 21,
    convRule: rule92
  }, {
    start: 1632,
    length: 10,
    convRule: rule8
  }, {
    start: 1642,
    length: 4,
    convRule: rule2
  }, {
    start: 1646,
    length: 2,
    convRule: rule14
  }, {
    start: 1648,
    length: 1,
    convRule: rule92
  }, {
    start: 1649,
    length: 99,
    convRule: rule14
  }, {
    start: 1748,
    length: 1,
    convRule: rule2
  }, {
    start: 1749,
    length: 1,
    convRule: rule14
  }, {
    start: 1750,
    length: 7,
    convRule: rule92
  }, {
    start: 1757,
    length: 1,
    convRule: rule16
  }, {
    start: 1758,
    length: 1,
    convRule: rule13
  }, {
    start: 1759,
    length: 6,
    convRule: rule92
  }, {
    start: 1765,
    length: 2,
    convRule: rule91
  }, {
    start: 1767,
    length: 2,
    convRule: rule92
  }, {
    start: 1769,
    length: 1,
    convRule: rule13
  }, {
    start: 1770,
    length: 4,
    convRule: rule92
  }, {
    start: 1774,
    length: 2,
    convRule: rule14
  }, {
    start: 1776,
    length: 10,
    convRule: rule8
  }, {
    start: 1786,
    length: 3,
    convRule: rule14
  }, {
    start: 1789,
    length: 2,
    convRule: rule13
  }, {
    start: 1791,
    length: 1,
    convRule: rule14
  }, {
    start: 1792,
    length: 14,
    convRule: rule2
  }, {
    start: 1807,
    length: 1,
    convRule: rule16
  }, {
    start: 1808,
    length: 1,
    convRule: rule14
  }, {
    start: 1809,
    length: 1,
    convRule: rule92
  }, {
    start: 1810,
    length: 30,
    convRule: rule14
  }, {
    start: 1840,
    length: 27,
    convRule: rule92
  }, {
    start: 1869,
    length: 89,
    convRule: rule14
  }, {
    start: 1958,
    length: 11,
    convRule: rule92
  }, {
    start: 1969,
    length: 1,
    convRule: rule14
  }, {
    start: 1984,
    length: 10,
    convRule: rule8
  }, {
    start: 1994,
    length: 33,
    convRule: rule14
  }, {
    start: 2027,
    length: 9,
    convRule: rule92
  }, {
    start: 2036,
    length: 2,
    convRule: rule91
  }, {
    start: 2038,
    length: 1,
    convRule: rule13
  }, {
    start: 2039,
    length: 3,
    convRule: rule2
  }, {
    start: 2042,
    length: 1,
    convRule: rule91
  }, {
    start: 2045,
    length: 1,
    convRule: rule92
  }, {
    start: 2046,
    length: 2,
    convRule: rule3
  }, {
    start: 2048,
    length: 22,
    convRule: rule14
  }, {
    start: 2070,
    length: 4,
    convRule: rule92
  }, {
    start: 2074,
    length: 1,
    convRule: rule91
  }, {
    start: 2075,
    length: 9,
    convRule: rule92
  }, {
    start: 2084,
    length: 1,
    convRule: rule91
  }, {
    start: 2085,
    length: 3,
    convRule: rule92
  }, {
    start: 2088,
    length: 1,
    convRule: rule91
  }, {
    start: 2089,
    length: 5,
    convRule: rule92
  }, {
    start: 2096,
    length: 15,
    convRule: rule2
  }, {
    start: 2112,
    length: 25,
    convRule: rule14
  }, {
    start: 2137,
    length: 3,
    convRule: rule92
  }, {
    start: 2142,
    length: 1,
    convRule: rule2
  }, {
    start: 2144,
    length: 11,
    convRule: rule14
  }, {
    start: 2208,
    length: 21,
    convRule: rule14
  }, {
    start: 2230,
    length: 18,
    convRule: rule14
  }, {
    start: 2259,
    length: 15,
    convRule: rule92
  }, {
    start: 2274,
    length: 1,
    convRule: rule16
  }, {
    start: 2275,
    length: 32,
    convRule: rule92
  }, {
    start: 2307,
    length: 1,
    convRule: rule124
  }, {
    start: 2308,
    length: 54,
    convRule: rule14
  }, {
    start: 2362,
    length: 1,
    convRule: rule92
  }, {
    start: 2363,
    length: 1,
    convRule: rule124
  }, {
    start: 2364,
    length: 1,
    convRule: rule92
  }, {
    start: 2365,
    length: 1,
    convRule: rule14
  }, {
    start: 2366,
    length: 3,
    convRule: rule124
  }, {
    start: 2369,
    length: 8,
    convRule: rule92
  }, {
    start: 2377,
    length: 4,
    convRule: rule124
  }, {
    start: 2381,
    length: 1,
    convRule: rule92
  }, {
    start: 2382,
    length: 2,
    convRule: rule124
  }, {
    start: 2384,
    length: 1,
    convRule: rule14
  }, {
    start: 2385,
    length: 7,
    convRule: rule92
  }, {
    start: 2392,
    length: 10,
    convRule: rule14
  }, {
    start: 2402,
    length: 2,
    convRule: rule92
  }, {
    start: 2404,
    length: 2,
    convRule: rule2
  }, {
    start: 2406,
    length: 10,
    convRule: rule8
  }, {
    start: 2416,
    length: 1,
    convRule: rule2
  }, {
    start: 2417,
    length: 1,
    convRule: rule91
  }, {
    start: 2418,
    length: 15,
    convRule: rule14
  }, {
    start: 2433,
    length: 1,
    convRule: rule92
  }, {
    start: 2434,
    length: 2,
    convRule: rule124
  }, {
    start: 2437,
    length: 8,
    convRule: rule14
  }, {
    start: 2447,
    length: 2,
    convRule: rule14
  }, {
    start: 2451,
    length: 22,
    convRule: rule14
  }, {
    start: 2474,
    length: 7,
    convRule: rule14
  }, {
    start: 2482,
    length: 1,
    convRule: rule14
  }, {
    start: 2486,
    length: 4,
    convRule: rule14
  }, {
    start: 2492,
    length: 1,
    convRule: rule92
  }, {
    start: 2493,
    length: 1,
    convRule: rule14
  }, {
    start: 2494,
    length: 3,
    convRule: rule124
  }, {
    start: 2497,
    length: 4,
    convRule: rule92
  }, {
    start: 2503,
    length: 2,
    convRule: rule124
  }, {
    start: 2507,
    length: 2,
    convRule: rule124
  }, {
    start: 2509,
    length: 1,
    convRule: rule92
  }, {
    start: 2510,
    length: 1,
    convRule: rule14
  }, {
    start: 2519,
    length: 1,
    convRule: rule124
  }, {
    start: 2524,
    length: 2,
    convRule: rule14
  }, {
    start: 2527,
    length: 3,
    convRule: rule14
  }, {
    start: 2530,
    length: 2,
    convRule: rule92
  }, {
    start: 2534,
    length: 10,
    convRule: rule8
  }, {
    start: 2544,
    length: 2,
    convRule: rule14
  }, {
    start: 2546,
    length: 2,
    convRule: rule3
  }, {
    start: 2548,
    length: 6,
    convRule: rule17
  }, {
    start: 2554,
    length: 1,
    convRule: rule13
  }, {
    start: 2555,
    length: 1,
    convRule: rule3
  }, {
    start: 2556,
    length: 1,
    convRule: rule14
  }, {
    start: 2557,
    length: 1,
    convRule: rule2
  }, {
    start: 2558,
    length: 1,
    convRule: rule92
  }, {
    start: 2561,
    length: 2,
    convRule: rule92
  }, {
    start: 2563,
    length: 1,
    convRule: rule124
  }, {
    start: 2565,
    length: 6,
    convRule: rule14
  }, {
    start: 2575,
    length: 2,
    convRule: rule14
  }, {
    start: 2579,
    length: 22,
    convRule: rule14
  }, {
    start: 2602,
    length: 7,
    convRule: rule14
  }, {
    start: 2610,
    length: 2,
    convRule: rule14
  }, {
    start: 2613,
    length: 2,
    convRule: rule14
  }, {
    start: 2616,
    length: 2,
    convRule: rule14
  }, {
    start: 2620,
    length: 1,
    convRule: rule92
  }, {
    start: 2622,
    length: 3,
    convRule: rule124
  }, {
    start: 2625,
    length: 2,
    convRule: rule92
  }, {
    start: 2631,
    length: 2,
    convRule: rule92
  }, {
    start: 2635,
    length: 3,
    convRule: rule92
  }, {
    start: 2641,
    length: 1,
    convRule: rule92
  }, {
    start: 2649,
    length: 4,
    convRule: rule14
  }, {
    start: 2654,
    length: 1,
    convRule: rule14
  }, {
    start: 2662,
    length: 10,
    convRule: rule8
  }, {
    start: 2672,
    length: 2,
    convRule: rule92
  }, {
    start: 2674,
    length: 3,
    convRule: rule14
  }, {
    start: 2677,
    length: 1,
    convRule: rule92
  }, {
    start: 2678,
    length: 1,
    convRule: rule2
  }, {
    start: 2689,
    length: 2,
    convRule: rule92
  }, {
    start: 2691,
    length: 1,
    convRule: rule124
  }, {
    start: 2693,
    length: 9,
    convRule: rule14
  }, {
    start: 2703,
    length: 3,
    convRule: rule14
  }, {
    start: 2707,
    length: 22,
    convRule: rule14
  }, {
    start: 2730,
    length: 7,
    convRule: rule14
  }, {
    start: 2738,
    length: 2,
    convRule: rule14
  }, {
    start: 2741,
    length: 5,
    convRule: rule14
  }, {
    start: 2748,
    length: 1,
    convRule: rule92
  }, {
    start: 2749,
    length: 1,
    convRule: rule14
  }, {
    start: 2750,
    length: 3,
    convRule: rule124
  }, {
    start: 2753,
    length: 5,
    convRule: rule92
  }, {
    start: 2759,
    length: 2,
    convRule: rule92
  }, {
    start: 2761,
    length: 1,
    convRule: rule124
  }, {
    start: 2763,
    length: 2,
    convRule: rule124
  }, {
    start: 2765,
    length: 1,
    convRule: rule92
  }, {
    start: 2768,
    length: 1,
    convRule: rule14
  }, {
    start: 2784,
    length: 2,
    convRule: rule14
  }, {
    start: 2786,
    length: 2,
    convRule: rule92
  }, {
    start: 2790,
    length: 10,
    convRule: rule8
  }, {
    start: 2800,
    length: 1,
    convRule: rule2
  }, {
    start: 2801,
    length: 1,
    convRule: rule3
  }, {
    start: 2809,
    length: 1,
    convRule: rule14
  }, {
    start: 2810,
    length: 6,
    convRule: rule92
  }, {
    start: 2817,
    length: 1,
    convRule: rule92
  }, {
    start: 2818,
    length: 2,
    convRule: rule124
  }, {
    start: 2821,
    length: 8,
    convRule: rule14
  }, {
    start: 2831,
    length: 2,
    convRule: rule14
  }, {
    start: 2835,
    length: 22,
    convRule: rule14
  }, {
    start: 2858,
    length: 7,
    convRule: rule14
  }, {
    start: 2866,
    length: 2,
    convRule: rule14
  }, {
    start: 2869,
    length: 5,
    convRule: rule14
  }, {
    start: 2876,
    length: 1,
    convRule: rule92
  }, {
    start: 2877,
    length: 1,
    convRule: rule14
  }, {
    start: 2878,
    length: 1,
    convRule: rule124
  }, {
    start: 2879,
    length: 1,
    convRule: rule92
  }, {
    start: 2880,
    length: 1,
    convRule: rule124
  }, {
    start: 2881,
    length: 4,
    convRule: rule92
  }, {
    start: 2887,
    length: 2,
    convRule: rule124
  }, {
    start: 2891,
    length: 2,
    convRule: rule124
  }, {
    start: 2893,
    length: 1,
    convRule: rule92
  }, {
    start: 2901,
    length: 2,
    convRule: rule92
  }, {
    start: 2903,
    length: 1,
    convRule: rule124
  }, {
    start: 2908,
    length: 2,
    convRule: rule14
  }, {
    start: 2911,
    length: 3,
    convRule: rule14
  }, {
    start: 2914,
    length: 2,
    convRule: rule92
  }, {
    start: 2918,
    length: 10,
    convRule: rule8
  }, {
    start: 2928,
    length: 1,
    convRule: rule13
  }, {
    start: 2929,
    length: 1,
    convRule: rule14
  }, {
    start: 2930,
    length: 6,
    convRule: rule17
  }, {
    start: 2946,
    length: 1,
    convRule: rule92
  }, {
    start: 2947,
    length: 1,
    convRule: rule14
  }, {
    start: 2949,
    length: 6,
    convRule: rule14
  }, {
    start: 2958,
    length: 3,
    convRule: rule14
  }, {
    start: 2962,
    length: 4,
    convRule: rule14
  }, {
    start: 2969,
    length: 2,
    convRule: rule14
  }, {
    start: 2972,
    length: 1,
    convRule: rule14
  }, {
    start: 2974,
    length: 2,
    convRule: rule14
  }, {
    start: 2979,
    length: 2,
    convRule: rule14
  }, {
    start: 2984,
    length: 3,
    convRule: rule14
  }, {
    start: 2990,
    length: 12,
    convRule: rule14
  }, {
    start: 3006,
    length: 2,
    convRule: rule124
  }, {
    start: 3008,
    length: 1,
    convRule: rule92
  }, {
    start: 3009,
    length: 2,
    convRule: rule124
  }, {
    start: 3014,
    length: 3,
    convRule: rule124
  }, {
    start: 3018,
    length: 3,
    convRule: rule124
  }, {
    start: 3021,
    length: 1,
    convRule: rule92
  }, {
    start: 3024,
    length: 1,
    convRule: rule14
  }, {
    start: 3031,
    length: 1,
    convRule: rule124
  }, {
    start: 3046,
    length: 10,
    convRule: rule8
  }, {
    start: 3056,
    length: 3,
    convRule: rule17
  }, {
    start: 3059,
    length: 6,
    convRule: rule13
  }, {
    start: 3065,
    length: 1,
    convRule: rule3
  }, {
    start: 3066,
    length: 1,
    convRule: rule13
  }, {
    start: 3072,
    length: 1,
    convRule: rule92
  }, {
    start: 3073,
    length: 3,
    convRule: rule124
  }, {
    start: 3076,
    length: 1,
    convRule: rule92
  }, {
    start: 3077,
    length: 8,
    convRule: rule14
  }, {
    start: 3086,
    length: 3,
    convRule: rule14
  }, {
    start: 3090,
    length: 23,
    convRule: rule14
  }, {
    start: 3114,
    length: 16,
    convRule: rule14
  }, {
    start: 3133,
    length: 1,
    convRule: rule14
  }, {
    start: 3134,
    length: 3,
    convRule: rule92
  }, {
    start: 3137,
    length: 4,
    convRule: rule124
  }, {
    start: 3142,
    length: 3,
    convRule: rule92
  }, {
    start: 3146,
    length: 4,
    convRule: rule92
  }, {
    start: 3157,
    length: 2,
    convRule: rule92
  }, {
    start: 3160,
    length: 3,
    convRule: rule14
  }, {
    start: 3168,
    length: 2,
    convRule: rule14
  }, {
    start: 3170,
    length: 2,
    convRule: rule92
  }, {
    start: 3174,
    length: 10,
    convRule: rule8
  }, {
    start: 3191,
    length: 1,
    convRule: rule2
  }, {
    start: 3192,
    length: 7,
    convRule: rule17
  }, {
    start: 3199,
    length: 1,
    convRule: rule13
  }, {
    start: 3200,
    length: 1,
    convRule: rule14
  }, {
    start: 3201,
    length: 1,
    convRule: rule92
  }, {
    start: 3202,
    length: 2,
    convRule: rule124
  }, {
    start: 3204,
    length: 1,
    convRule: rule2
  }, {
    start: 3205,
    length: 8,
    convRule: rule14
  }, {
    start: 3214,
    length: 3,
    convRule: rule14
  }, {
    start: 3218,
    length: 23,
    convRule: rule14
  }, {
    start: 3242,
    length: 10,
    convRule: rule14
  }, {
    start: 3253,
    length: 5,
    convRule: rule14
  }, {
    start: 3260,
    length: 1,
    convRule: rule92
  }, {
    start: 3261,
    length: 1,
    convRule: rule14
  }, {
    start: 3262,
    length: 1,
    convRule: rule124
  }, {
    start: 3263,
    length: 1,
    convRule: rule92
  }, {
    start: 3264,
    length: 5,
    convRule: rule124
  }, {
    start: 3270,
    length: 1,
    convRule: rule92
  }, {
    start: 3271,
    length: 2,
    convRule: rule124
  }, {
    start: 3274,
    length: 2,
    convRule: rule124
  }, {
    start: 3276,
    length: 2,
    convRule: rule92
  }, {
    start: 3285,
    length: 2,
    convRule: rule124
  }, {
    start: 3294,
    length: 1,
    convRule: rule14
  }, {
    start: 3296,
    length: 2,
    convRule: rule14
  }, {
    start: 3298,
    length: 2,
    convRule: rule92
  }, {
    start: 3302,
    length: 10,
    convRule: rule8
  }, {
    start: 3313,
    length: 2,
    convRule: rule14
  }, {
    start: 3328,
    length: 2,
    convRule: rule92
  }, {
    start: 3330,
    length: 2,
    convRule: rule124
  }, {
    start: 3332,
    length: 9,
    convRule: rule14
  }, {
    start: 3342,
    length: 3,
    convRule: rule14
  }, {
    start: 3346,
    length: 41,
    convRule: rule14
  }, {
    start: 3387,
    length: 2,
    convRule: rule92
  }, {
    start: 3389,
    length: 1,
    convRule: rule14
  }, {
    start: 3390,
    length: 3,
    convRule: rule124
  }, {
    start: 3393,
    length: 4,
    convRule: rule92
  }, {
    start: 3398,
    length: 3,
    convRule: rule124
  }, {
    start: 3402,
    length: 3,
    convRule: rule124
  }, {
    start: 3405,
    length: 1,
    convRule: rule92
  }, {
    start: 3406,
    length: 1,
    convRule: rule14
  }, {
    start: 3407,
    length: 1,
    convRule: rule13
  }, {
    start: 3412,
    length: 3,
    convRule: rule14
  }, {
    start: 3415,
    length: 1,
    convRule: rule124
  }, {
    start: 3416,
    length: 7,
    convRule: rule17
  }, {
    start: 3423,
    length: 3,
    convRule: rule14
  }, {
    start: 3426,
    length: 2,
    convRule: rule92
  }, {
    start: 3430,
    length: 10,
    convRule: rule8
  }, {
    start: 3440,
    length: 9,
    convRule: rule17
  }, {
    start: 3449,
    length: 1,
    convRule: rule13
  }, {
    start: 3450,
    length: 6,
    convRule: rule14
  }, {
    start: 3457,
    length: 1,
    convRule: rule92
  }, {
    start: 3458,
    length: 2,
    convRule: rule124
  }, {
    start: 3461,
    length: 18,
    convRule: rule14
  }, {
    start: 3482,
    length: 24,
    convRule: rule14
  }, {
    start: 3507,
    length: 9,
    convRule: rule14
  }, {
    start: 3517,
    length: 1,
    convRule: rule14
  }, {
    start: 3520,
    length: 7,
    convRule: rule14
  }, {
    start: 3530,
    length: 1,
    convRule: rule92
  }, {
    start: 3535,
    length: 3,
    convRule: rule124
  }, {
    start: 3538,
    length: 3,
    convRule: rule92
  }, {
    start: 3542,
    length: 1,
    convRule: rule92
  }, {
    start: 3544,
    length: 8,
    convRule: rule124
  }, {
    start: 3558,
    length: 10,
    convRule: rule8
  }, {
    start: 3570,
    length: 2,
    convRule: rule124
  }, {
    start: 3572,
    length: 1,
    convRule: rule2
  }, {
    start: 3585,
    length: 48,
    convRule: rule14
  }, {
    start: 3633,
    length: 1,
    convRule: rule92
  }, {
    start: 3634,
    length: 2,
    convRule: rule14
  }, {
    start: 3636,
    length: 7,
    convRule: rule92
  }, {
    start: 3647,
    length: 1,
    convRule: rule3
  }, {
    start: 3648,
    length: 6,
    convRule: rule14
  }, {
    start: 3654,
    length: 1,
    convRule: rule91
  }, {
    start: 3655,
    length: 8,
    convRule: rule92
  }, {
    start: 3663,
    length: 1,
    convRule: rule2
  }, {
    start: 3664,
    length: 10,
    convRule: rule8
  }, {
    start: 3674,
    length: 2,
    convRule: rule2
  }, {
    start: 3713,
    length: 2,
    convRule: rule14
  }, {
    start: 3716,
    length: 1,
    convRule: rule14
  }, {
    start: 3718,
    length: 5,
    convRule: rule14
  }, {
    start: 3724,
    length: 24,
    convRule: rule14
  }, {
    start: 3749,
    length: 1,
    convRule: rule14
  }, {
    start: 3751,
    length: 10,
    convRule: rule14
  }, {
    start: 3761,
    length: 1,
    convRule: rule92
  }, {
    start: 3762,
    length: 2,
    convRule: rule14
  }, {
    start: 3764,
    length: 9,
    convRule: rule92
  }, {
    start: 3773,
    length: 1,
    convRule: rule14
  }, {
    start: 3776,
    length: 5,
    convRule: rule14
  }, {
    start: 3782,
    length: 1,
    convRule: rule91
  }, {
    start: 3784,
    length: 6,
    convRule: rule92
  }, {
    start: 3792,
    length: 10,
    convRule: rule8
  }, {
    start: 3804,
    length: 4,
    convRule: rule14
  }, {
    start: 3840,
    length: 1,
    convRule: rule14
  }, {
    start: 3841,
    length: 3,
    convRule: rule13
  }, {
    start: 3844,
    length: 15,
    convRule: rule2
  }, {
    start: 3859,
    length: 1,
    convRule: rule13
  }, {
    start: 3860,
    length: 1,
    convRule: rule2
  }, {
    start: 3861,
    length: 3,
    convRule: rule13
  }, {
    start: 3864,
    length: 2,
    convRule: rule92
  }, {
    start: 3866,
    length: 6,
    convRule: rule13
  }, {
    start: 3872,
    length: 10,
    convRule: rule8
  }, {
    start: 3882,
    length: 10,
    convRule: rule17
  }, {
    start: 3892,
    length: 1,
    convRule: rule13
  }, {
    start: 3893,
    length: 1,
    convRule: rule92
  }, {
    start: 3894,
    length: 1,
    convRule: rule13
  }, {
    start: 3895,
    length: 1,
    convRule: rule92
  }, {
    start: 3896,
    length: 1,
    convRule: rule13
  }, {
    start: 3897,
    length: 1,
    convRule: rule92
  }, {
    start: 3898,
    length: 1,
    convRule: rule4
  }, {
    start: 3899,
    length: 1,
    convRule: rule5
  }, {
    start: 3900,
    length: 1,
    convRule: rule4
  }, {
    start: 3901,
    length: 1,
    convRule: rule5
  }, {
    start: 3902,
    length: 2,
    convRule: rule124
  }, {
    start: 3904,
    length: 8,
    convRule: rule14
  }, {
    start: 3913,
    length: 36,
    convRule: rule14
  }, {
    start: 3953,
    length: 14,
    convRule: rule92
  }, {
    start: 3967,
    length: 1,
    convRule: rule124
  }, {
    start: 3968,
    length: 5,
    convRule: rule92
  }, {
    start: 3973,
    length: 1,
    convRule: rule2
  }, {
    start: 3974,
    length: 2,
    convRule: rule92
  }, {
    start: 3976,
    length: 5,
    convRule: rule14
  }, {
    start: 3981,
    length: 11,
    convRule: rule92
  }, {
    start: 3993,
    length: 36,
    convRule: rule92
  }, {
    start: 4030,
    length: 8,
    convRule: rule13
  }, {
    start: 4038,
    length: 1,
    convRule: rule92
  }, {
    start: 4039,
    length: 6,
    convRule: rule13
  }, {
    start: 4046,
    length: 2,
    convRule: rule13
  }, {
    start: 4048,
    length: 5,
    convRule: rule2
  }, {
    start: 4053,
    length: 4,
    convRule: rule13
  }, {
    start: 4057,
    length: 2,
    convRule: rule2
  }, {
    start: 4096,
    length: 43,
    convRule: rule14
  }, {
    start: 4139,
    length: 2,
    convRule: rule124
  }, {
    start: 4141,
    length: 4,
    convRule: rule92
  }, {
    start: 4145,
    length: 1,
    convRule: rule124
  }, {
    start: 4146,
    length: 6,
    convRule: rule92
  }, {
    start: 4152,
    length: 1,
    convRule: rule124
  }, {
    start: 4153,
    length: 2,
    convRule: rule92
  }, {
    start: 4155,
    length: 2,
    convRule: rule124
  }, {
    start: 4157,
    length: 2,
    convRule: rule92
  }, {
    start: 4159,
    length: 1,
    convRule: rule14
  }, {
    start: 4160,
    length: 10,
    convRule: rule8
  }, {
    start: 4170,
    length: 6,
    convRule: rule2
  }, {
    start: 4176,
    length: 6,
    convRule: rule14
  }, {
    start: 4182,
    length: 2,
    convRule: rule124
  }, {
    start: 4184,
    length: 2,
    convRule: rule92
  }, {
    start: 4186,
    length: 4,
    convRule: rule14
  }, {
    start: 4190,
    length: 3,
    convRule: rule92
  }, {
    start: 4193,
    length: 1,
    convRule: rule14
  }, {
    start: 4194,
    length: 3,
    convRule: rule124
  }, {
    start: 4197,
    length: 2,
    convRule: rule14
  }, {
    start: 4199,
    length: 7,
    convRule: rule124
  }, {
    start: 4206,
    length: 3,
    convRule: rule14
  }, {
    start: 4209,
    length: 4,
    convRule: rule92
  }, {
    start: 4213,
    length: 13,
    convRule: rule14
  }, {
    start: 4226,
    length: 1,
    convRule: rule92
  }, {
    start: 4227,
    length: 2,
    convRule: rule124
  }, {
    start: 4229,
    length: 2,
    convRule: rule92
  }, {
    start: 4231,
    length: 6,
    convRule: rule124
  }, {
    start: 4237,
    length: 1,
    convRule: rule92
  }, {
    start: 4238,
    length: 1,
    convRule: rule14
  }, {
    start: 4239,
    length: 1,
    convRule: rule124
  }, {
    start: 4240,
    length: 10,
    convRule: rule8
  }, {
    start: 4250,
    length: 3,
    convRule: rule124
  }, {
    start: 4253,
    length: 1,
    convRule: rule92
  }, {
    start: 4254,
    length: 2,
    convRule: rule13
  }, {
    start: 4256,
    length: 38,
    convRule: rule125
  }, {
    start: 4295,
    length: 1,
    convRule: rule125
  }, {
    start: 4301,
    length: 1,
    convRule: rule125
  }, {
    start: 4304,
    length: 43,
    convRule: rule126
  }, {
    start: 4347,
    length: 1,
    convRule: rule2
  }, {
    start: 4348,
    length: 1,
    convRule: rule91
  }, {
    start: 4349,
    length: 3,
    convRule: rule126
  }, {
    start: 4352,
    length: 329,
    convRule: rule14
  }, {
    start: 4682,
    length: 4,
    convRule: rule14
  }, {
    start: 4688,
    length: 7,
    convRule: rule14
  }, {
    start: 4696,
    length: 1,
    convRule: rule14
  }, {
    start: 4698,
    length: 4,
    convRule: rule14
  }, {
    start: 4704,
    length: 41,
    convRule: rule14
  }, {
    start: 4746,
    length: 4,
    convRule: rule14
  }, {
    start: 4752,
    length: 33,
    convRule: rule14
  }, {
    start: 4786,
    length: 4,
    convRule: rule14
  }, {
    start: 4792,
    length: 7,
    convRule: rule14
  }, {
    start: 4800,
    length: 1,
    convRule: rule14
  }, {
    start: 4802,
    length: 4,
    convRule: rule14
  }, {
    start: 4808,
    length: 15,
    convRule: rule14
  }, {
    start: 4824,
    length: 57,
    convRule: rule14
  }, {
    start: 4882,
    length: 4,
    convRule: rule14
  }, {
    start: 4888,
    length: 67,
    convRule: rule14
  }, {
    start: 4957,
    length: 3,
    convRule: rule92
  }, {
    start: 4960,
    length: 9,
    convRule: rule2
  }, {
    start: 4969,
    length: 20,
    convRule: rule17
  }, {
    start: 4992,
    length: 16,
    convRule: rule14
  }, {
    start: 5008,
    length: 10,
    convRule: rule13
  }, {
    start: 5024,
    length: 80,
    convRule: rule127
  }, {
    start: 5104,
    length: 6,
    convRule: rule104
  }, {
    start: 5112,
    length: 6,
    convRule: rule110
  }, {
    start: 5120,
    length: 1,
    convRule: rule7
  }, {
    start: 5121,
    length: 620,
    convRule: rule14
  }, {
    start: 5741,
    length: 1,
    convRule: rule13
  }, {
    start: 5742,
    length: 1,
    convRule: rule2
  }, {
    start: 5743,
    length: 17,
    convRule: rule14
  }, {
    start: 5760,
    length: 1,
    convRule: rule1
  }, {
    start: 5761,
    length: 26,
    convRule: rule14
  }, {
    start: 5787,
    length: 1,
    convRule: rule4
  }, {
    start: 5788,
    length: 1,
    convRule: rule5
  }, {
    start: 5792,
    length: 75,
    convRule: rule14
  }, {
    start: 5867,
    length: 3,
    convRule: rule2
  }, {
    start: 5870,
    length: 3,
    convRule: rule128
  }, {
    start: 5873,
    length: 8,
    convRule: rule14
  }, {
    start: 5888,
    length: 13,
    convRule: rule14
  }, {
    start: 5902,
    length: 4,
    convRule: rule14
  }, {
    start: 5906,
    length: 3,
    convRule: rule92
  }, {
    start: 5920,
    length: 18,
    convRule: rule14
  }, {
    start: 5938,
    length: 3,
    convRule: rule92
  }, {
    start: 5941,
    length: 2,
    convRule: rule2
  }, {
    start: 5952,
    length: 18,
    convRule: rule14
  }, {
    start: 5970,
    length: 2,
    convRule: rule92
  }, {
    start: 5984,
    length: 13,
    convRule: rule14
  }, {
    start: 5998,
    length: 3,
    convRule: rule14
  }, {
    start: 6002,
    length: 2,
    convRule: rule92
  }, {
    start: 6016,
    length: 52,
    convRule: rule14
  }, {
    start: 6068,
    length: 2,
    convRule: rule92
  }, {
    start: 6070,
    length: 1,
    convRule: rule124
  }, {
    start: 6071,
    length: 7,
    convRule: rule92
  }, {
    start: 6078,
    length: 8,
    convRule: rule124
  }, {
    start: 6086,
    length: 1,
    convRule: rule92
  }, {
    start: 6087,
    length: 2,
    convRule: rule124
  }, {
    start: 6089,
    length: 11,
    convRule: rule92
  }, {
    start: 6100,
    length: 3,
    convRule: rule2
  }, {
    start: 6103,
    length: 1,
    convRule: rule91
  }, {
    start: 6104,
    length: 3,
    convRule: rule2
  }, {
    start: 6107,
    length: 1,
    convRule: rule3
  }, {
    start: 6108,
    length: 1,
    convRule: rule14
  }, {
    start: 6109,
    length: 1,
    convRule: rule92
  }, {
    start: 6112,
    length: 10,
    convRule: rule8
  }, {
    start: 6128,
    length: 10,
    convRule: rule17
  }, {
    start: 6144,
    length: 6,
    convRule: rule2
  }, {
    start: 6150,
    length: 1,
    convRule: rule7
  }, {
    start: 6151,
    length: 4,
    convRule: rule2
  }, {
    start: 6155,
    length: 3,
    convRule: rule92
  }, {
    start: 6158,
    length: 1,
    convRule: rule16
  }, {
    start: 6160,
    length: 10,
    convRule: rule8
  }, {
    start: 6176,
    length: 35,
    convRule: rule14
  }, {
    start: 6211,
    length: 1,
    convRule: rule91
  }, {
    start: 6212,
    length: 53,
    convRule: rule14
  }, {
    start: 6272,
    length: 5,
    convRule: rule14
  }, {
    start: 6277,
    length: 2,
    convRule: rule92
  }, {
    start: 6279,
    length: 34,
    convRule: rule14
  }, {
    start: 6313,
    length: 1,
    convRule: rule92
  }, {
    start: 6314,
    length: 1,
    convRule: rule14
  }, {
    start: 6320,
    length: 70,
    convRule: rule14
  }, {
    start: 6400,
    length: 31,
    convRule: rule14
  }, {
    start: 6432,
    length: 3,
    convRule: rule92
  }, {
    start: 6435,
    length: 4,
    convRule: rule124
  }, {
    start: 6439,
    length: 2,
    convRule: rule92
  }, {
    start: 6441,
    length: 3,
    convRule: rule124
  }, {
    start: 6448,
    length: 2,
    convRule: rule124
  }, {
    start: 6450,
    length: 1,
    convRule: rule92
  }, {
    start: 6451,
    length: 6,
    convRule: rule124
  }, {
    start: 6457,
    length: 3,
    convRule: rule92
  }, {
    start: 6464,
    length: 1,
    convRule: rule13
  }, {
    start: 6468,
    length: 2,
    convRule: rule2
  }, {
    start: 6470,
    length: 10,
    convRule: rule8
  }, {
    start: 6480,
    length: 30,
    convRule: rule14
  }, {
    start: 6512,
    length: 5,
    convRule: rule14
  }, {
    start: 6528,
    length: 44,
    convRule: rule14
  }, {
    start: 6576,
    length: 26,
    convRule: rule14
  }, {
    start: 6608,
    length: 10,
    convRule: rule8
  }, {
    start: 6618,
    length: 1,
    convRule: rule17
  }, {
    start: 6622,
    length: 34,
    convRule: rule13
  }, {
    start: 6656,
    length: 23,
    convRule: rule14
  }, {
    start: 6679,
    length: 2,
    convRule: rule92
  }, {
    start: 6681,
    length: 2,
    convRule: rule124
  }, {
    start: 6683,
    length: 1,
    convRule: rule92
  }, {
    start: 6686,
    length: 2,
    convRule: rule2
  }, {
    start: 6688,
    length: 53,
    convRule: rule14
  }, {
    start: 6741,
    length: 1,
    convRule: rule124
  }, {
    start: 6742,
    length: 1,
    convRule: rule92
  }, {
    start: 6743,
    length: 1,
    convRule: rule124
  }, {
    start: 6744,
    length: 7,
    convRule: rule92
  }, {
    start: 6752,
    length: 1,
    convRule: rule92
  }, {
    start: 6753,
    length: 1,
    convRule: rule124
  }, {
    start: 6754,
    length: 1,
    convRule: rule92
  }, {
    start: 6755,
    length: 2,
    convRule: rule124
  }, {
    start: 6757,
    length: 8,
    convRule: rule92
  }, {
    start: 6765,
    length: 6,
    convRule: rule124
  }, {
    start: 6771,
    length: 10,
    convRule: rule92
  }, {
    start: 6783,
    length: 1,
    convRule: rule92
  }, {
    start: 6784,
    length: 10,
    convRule: rule8
  }, {
    start: 6800,
    length: 10,
    convRule: rule8
  }, {
    start: 6816,
    length: 7,
    convRule: rule2
  }, {
    start: 6823,
    length: 1,
    convRule: rule91
  }, {
    start: 6824,
    length: 6,
    convRule: rule2
  }, {
    start: 6832,
    length: 14,
    convRule: rule92
  }, {
    start: 6846,
    length: 1,
    convRule: rule119
  }, {
    start: 6847,
    length: 2,
    convRule: rule92
  }, {
    start: 6912,
    length: 4,
    convRule: rule92
  }, {
    start: 6916,
    length: 1,
    convRule: rule124
  }, {
    start: 6917,
    length: 47,
    convRule: rule14
  }, {
    start: 6964,
    length: 1,
    convRule: rule92
  }, {
    start: 6965,
    length: 1,
    convRule: rule124
  }, {
    start: 6966,
    length: 5,
    convRule: rule92
  }, {
    start: 6971,
    length: 1,
    convRule: rule124
  }, {
    start: 6972,
    length: 1,
    convRule: rule92
  }, {
    start: 6973,
    length: 5,
    convRule: rule124
  }, {
    start: 6978,
    length: 1,
    convRule: rule92
  }, {
    start: 6979,
    length: 2,
    convRule: rule124
  }, {
    start: 6981,
    length: 7,
    convRule: rule14
  }, {
    start: 6992,
    length: 10,
    convRule: rule8
  }, {
    start: 7002,
    length: 7,
    convRule: rule2
  }, {
    start: 7009,
    length: 10,
    convRule: rule13
  }, {
    start: 7019,
    length: 9,
    convRule: rule92
  }, {
    start: 7028,
    length: 9,
    convRule: rule13
  }, {
    start: 7040,
    length: 2,
    convRule: rule92
  }, {
    start: 7042,
    length: 1,
    convRule: rule124
  }, {
    start: 7043,
    length: 30,
    convRule: rule14
  }, {
    start: 7073,
    length: 1,
    convRule: rule124
  }, {
    start: 7074,
    length: 4,
    convRule: rule92
  }, {
    start: 7078,
    length: 2,
    convRule: rule124
  }, {
    start: 7080,
    length: 2,
    convRule: rule92
  }, {
    start: 7082,
    length: 1,
    convRule: rule124
  }, {
    start: 7083,
    length: 3,
    convRule: rule92
  }, {
    start: 7086,
    length: 2,
    convRule: rule14
  }, {
    start: 7088,
    length: 10,
    convRule: rule8
  }, {
    start: 7098,
    length: 44,
    convRule: rule14
  }, {
    start: 7142,
    length: 1,
    convRule: rule92
  }, {
    start: 7143,
    length: 1,
    convRule: rule124
  }, {
    start: 7144,
    length: 2,
    convRule: rule92
  }, {
    start: 7146,
    length: 3,
    convRule: rule124
  }, {
    start: 7149,
    length: 1,
    convRule: rule92
  }, {
    start: 7150,
    length: 1,
    convRule: rule124
  }, {
    start: 7151,
    length: 3,
    convRule: rule92
  }, {
    start: 7154,
    length: 2,
    convRule: rule124
  }, {
    start: 7164,
    length: 4,
    convRule: rule2
  }, {
    start: 7168,
    length: 36,
    convRule: rule14
  }, {
    start: 7204,
    length: 8,
    convRule: rule124
  }, {
    start: 7212,
    length: 8,
    convRule: rule92
  }, {
    start: 7220,
    length: 2,
    convRule: rule124
  }, {
    start: 7222,
    length: 2,
    convRule: rule92
  }, {
    start: 7227,
    length: 5,
    convRule: rule2
  }, {
    start: 7232,
    length: 10,
    convRule: rule8
  }, {
    start: 7245,
    length: 3,
    convRule: rule14
  }, {
    start: 7248,
    length: 10,
    convRule: rule8
  }, {
    start: 7258,
    length: 30,
    convRule: rule14
  }, {
    start: 7288,
    length: 6,
    convRule: rule91
  }, {
    start: 7294,
    length: 2,
    convRule: rule2
  }, {
    start: 7296,
    length: 1,
    convRule: rule129
  }, {
    start: 7297,
    length: 1,
    convRule: rule130
  }, {
    start: 7298,
    length: 1,
    convRule: rule131
  }, {
    start: 7299,
    length: 2,
    convRule: rule132
  }, {
    start: 7301,
    length: 1,
    convRule: rule133
  }, {
    start: 7302,
    length: 1,
    convRule: rule134
  }, {
    start: 7303,
    length: 1,
    convRule: rule135
  }, {
    start: 7304,
    length: 1,
    convRule: rule136
  }, {
    start: 7312,
    length: 43,
    convRule: rule137
  }, {
    start: 7357,
    length: 3,
    convRule: rule137
  }, {
    start: 7360,
    length: 8,
    convRule: rule2
  }, {
    start: 7376,
    length: 3,
    convRule: rule92
  }, {
    start: 7379,
    length: 1,
    convRule: rule2
  }, {
    start: 7380,
    length: 13,
    convRule: rule92
  }, {
    start: 7393,
    length: 1,
    convRule: rule124
  }, {
    start: 7394,
    length: 7,
    convRule: rule92
  }, {
    start: 7401,
    length: 4,
    convRule: rule14
  }, {
    start: 7405,
    length: 1,
    convRule: rule92
  }, {
    start: 7406,
    length: 6,
    convRule: rule14
  }, {
    start: 7412,
    length: 1,
    convRule: rule92
  }, {
    start: 7413,
    length: 2,
    convRule: rule14
  }, {
    start: 7415,
    length: 1,
    convRule: rule124
  }, {
    start: 7416,
    length: 2,
    convRule: rule92
  }, {
    start: 7418,
    length: 1,
    convRule: rule14
  }, {
    start: 7424,
    length: 44,
    convRule: rule20
  }, {
    start: 7468,
    length: 63,
    convRule: rule91
  }, {
    start: 7531,
    length: 13,
    convRule: rule20
  }, {
    start: 7544,
    length: 1,
    convRule: rule91
  }, {
    start: 7545,
    length: 1,
    convRule: rule138
  }, {
    start: 7546,
    length: 3,
    convRule: rule20
  }, {
    start: 7549,
    length: 1,
    convRule: rule139
  }, {
    start: 7550,
    length: 16,
    convRule: rule20
  }, {
    start: 7566,
    length: 1,
    convRule: rule140
  }, {
    start: 7567,
    length: 12,
    convRule: rule20
  }, {
    start: 7579,
    length: 37,
    convRule: rule91
  }, {
    start: 7616,
    length: 58,
    convRule: rule92
  }, {
    start: 7675,
    length: 5,
    convRule: rule92
  }, {
    start: 7680,
    length: 1,
    convRule: rule22
  }, {
    start: 7681,
    length: 1,
    convRule: rule23
  }, {
    start: 7682,
    length: 1,
    convRule: rule22
  }, {
    start: 7683,
    length: 1,
    convRule: rule23
  }, {
    start: 7684,
    length: 1,
    convRule: rule22
  }, {
    start: 7685,
    length: 1,
    convRule: rule23
  }, {
    start: 7686,
    length: 1,
    convRule: rule22
  }, {
    start: 7687,
    length: 1,
    convRule: rule23
  }, {
    start: 7688,
    length: 1,
    convRule: rule22
  }, {
    start: 7689,
    length: 1,
    convRule: rule23
  }, {
    start: 7690,
    length: 1,
    convRule: rule22
  }, {
    start: 7691,
    length: 1,
    convRule: rule23
  }, {
    start: 7692,
    length: 1,
    convRule: rule22
  }, {
    start: 7693,
    length: 1,
    convRule: rule23
  }, {
    start: 7694,
    length: 1,
    convRule: rule22
  }, {
    start: 7695,
    length: 1,
    convRule: rule23
  }, {
    start: 7696,
    length: 1,
    convRule: rule22
  }, {
    start: 7697,
    length: 1,
    convRule: rule23
  }, {
    start: 7698,
    length: 1,
    convRule: rule22
  }, {
    start: 7699,
    length: 1,
    convRule: rule23
  }, {
    start: 7700,
    length: 1,
    convRule: rule22
  }, {
    start: 7701,
    length: 1,
    convRule: rule23
  }, {
    start: 7702,
    length: 1,
    convRule: rule22
  }, {
    start: 7703,
    length: 1,
    convRule: rule23
  }, {
    start: 7704,
    length: 1,
    convRule: rule22
  }, {
    start: 7705,
    length: 1,
    convRule: rule23
  }, {
    start: 7706,
    length: 1,
    convRule: rule22
  }, {
    start: 7707,
    length: 1,
    convRule: rule23
  }, {
    start: 7708,
    length: 1,
    convRule: rule22
  }, {
    start: 7709,
    length: 1,
    convRule: rule23
  }, {
    start: 7710,
    length: 1,
    convRule: rule22
  }, {
    start: 7711,
    length: 1,
    convRule: rule23
  }, {
    start: 7712,
    length: 1,
    convRule: rule22
  }, {
    start: 7713,
    length: 1,
    convRule: rule23
  }, {
    start: 7714,
    length: 1,
    convRule: rule22
  }, {
    start: 7715,
    length: 1,
    convRule: rule23
  }, {
    start: 7716,
    length: 1,
    convRule: rule22
  }, {
    start: 7717,
    length: 1,
    convRule: rule23
  }, {
    start: 7718,
    length: 1,
    convRule: rule22
  }, {
    start: 7719,
    length: 1,
    convRule: rule23
  }, {
    start: 7720,
    length: 1,
    convRule: rule22
  }, {
    start: 7721,
    length: 1,
    convRule: rule23
  }, {
    start: 7722,
    length: 1,
    convRule: rule22
  }, {
    start: 7723,
    length: 1,
    convRule: rule23
  }, {
    start: 7724,
    length: 1,
    convRule: rule22
  }, {
    start: 7725,
    length: 1,
    convRule: rule23
  }, {
    start: 7726,
    length: 1,
    convRule: rule22
  }, {
    start: 7727,
    length: 1,
    convRule: rule23
  }, {
    start: 7728,
    length: 1,
    convRule: rule22
  }, {
    start: 7729,
    length: 1,
    convRule: rule23
  }, {
    start: 7730,
    length: 1,
    convRule: rule22
  }, {
    start: 7731,
    length: 1,
    convRule: rule23
  }, {
    start: 7732,
    length: 1,
    convRule: rule22
  }, {
    start: 7733,
    length: 1,
    convRule: rule23
  }, {
    start: 7734,
    length: 1,
    convRule: rule22
  }, {
    start: 7735,
    length: 1,
    convRule: rule23
  }, {
    start: 7736,
    length: 1,
    convRule: rule22
  }, {
    start: 7737,
    length: 1,
    convRule: rule23
  }, {
    start: 7738,
    length: 1,
    convRule: rule22
  }, {
    start: 7739,
    length: 1,
    convRule: rule23
  }, {
    start: 7740,
    length: 1,
    convRule: rule22
  }, {
    start: 7741,
    length: 1,
    convRule: rule23
  }, {
    start: 7742,
    length: 1,
    convRule: rule22
  }, {
    start: 7743,
    length: 1,
    convRule: rule23
  }, {
    start: 7744,
    length: 1,
    convRule: rule22
  }, {
    start: 7745,
    length: 1,
    convRule: rule23
  }, {
    start: 7746,
    length: 1,
    convRule: rule22
  }, {
    start: 7747,
    length: 1,
    convRule: rule23
  }, {
    start: 7748,
    length: 1,
    convRule: rule22
  }, {
    start: 7749,
    length: 1,
    convRule: rule23
  }, {
    start: 7750,
    length: 1,
    convRule: rule22
  }, {
    start: 7751,
    length: 1,
    convRule: rule23
  }, {
    start: 7752,
    length: 1,
    convRule: rule22
  }, {
    start: 7753,
    length: 1,
    convRule: rule23
  }, {
    start: 7754,
    length: 1,
    convRule: rule22
  }, {
    start: 7755,
    length: 1,
    convRule: rule23
  }, {
    start: 7756,
    length: 1,
    convRule: rule22
  }, {
    start: 7757,
    length: 1,
    convRule: rule23
  }, {
    start: 7758,
    length: 1,
    convRule: rule22
  }, {
    start: 7759,
    length: 1,
    convRule: rule23
  }, {
    start: 7760,
    length: 1,
    convRule: rule22
  }, {
    start: 7761,
    length: 1,
    convRule: rule23
  }, {
    start: 7762,
    length: 1,
    convRule: rule22
  }, {
    start: 7763,
    length: 1,
    convRule: rule23
  }, {
    start: 7764,
    length: 1,
    convRule: rule22
  }, {
    start: 7765,
    length: 1,
    convRule: rule23
  }, {
    start: 7766,
    length: 1,
    convRule: rule22
  }, {
    start: 7767,
    length: 1,
    convRule: rule23
  }, {
    start: 7768,
    length: 1,
    convRule: rule22
  }, {
    start: 7769,
    length: 1,
    convRule: rule23
  }, {
    start: 7770,
    length: 1,
    convRule: rule22
  }, {
    start: 7771,
    length: 1,
    convRule: rule23
  }, {
    start: 7772,
    length: 1,
    convRule: rule22
  }, {
    start: 7773,
    length: 1,
    convRule: rule23
  }, {
    start: 7774,
    length: 1,
    convRule: rule22
  }, {
    start: 7775,
    length: 1,
    convRule: rule23
  }, {
    start: 7776,
    length: 1,
    convRule: rule22
  }, {
    start: 7777,
    length: 1,
    convRule: rule23
  }, {
    start: 7778,
    length: 1,
    convRule: rule22
  }, {
    start: 7779,
    length: 1,
    convRule: rule23
  }, {
    start: 7780,
    length: 1,
    convRule: rule22
  }, {
    start: 7781,
    length: 1,
    convRule: rule23
  }, {
    start: 7782,
    length: 1,
    convRule: rule22
  }, {
    start: 7783,
    length: 1,
    convRule: rule23
  }, {
    start: 7784,
    length: 1,
    convRule: rule22
  }, {
    start: 7785,
    length: 1,
    convRule: rule23
  }, {
    start: 7786,
    length: 1,
    convRule: rule22
  }, {
    start: 7787,
    length: 1,
    convRule: rule23
  }, {
    start: 7788,
    length: 1,
    convRule: rule22
  }, {
    start: 7789,
    length: 1,
    convRule: rule23
  }, {
    start: 7790,
    length: 1,
    convRule: rule22
  }, {
    start: 7791,
    length: 1,
    convRule: rule23
  }, {
    start: 7792,
    length: 1,
    convRule: rule22
  }, {
    start: 7793,
    length: 1,
    convRule: rule23
  }, {
    start: 7794,
    length: 1,
    convRule: rule22
  }, {
    start: 7795,
    length: 1,
    convRule: rule23
  }, {
    start: 7796,
    length: 1,
    convRule: rule22
  }, {
    start: 7797,
    length: 1,
    convRule: rule23
  }, {
    start: 7798,
    length: 1,
    convRule: rule22
  }, {
    start: 7799,
    length: 1,
    convRule: rule23
  }, {
    start: 7800,
    length: 1,
    convRule: rule22
  }, {
    start: 7801,
    length: 1,
    convRule: rule23
  }, {
    start: 7802,
    length: 1,
    convRule: rule22
  }, {
    start: 7803,
    length: 1,
    convRule: rule23
  }, {
    start: 7804,
    length: 1,
    convRule: rule22
  }, {
    start: 7805,
    length: 1,
    convRule: rule23
  }, {
    start: 7806,
    length: 1,
    convRule: rule22
  }, {
    start: 7807,
    length: 1,
    convRule: rule23
  }, {
    start: 7808,
    length: 1,
    convRule: rule22
  }, {
    start: 7809,
    length: 1,
    convRule: rule23
  }, {
    start: 7810,
    length: 1,
    convRule: rule22
  }, {
    start: 7811,
    length: 1,
    convRule: rule23
  }, {
    start: 7812,
    length: 1,
    convRule: rule22
  }, {
    start: 7813,
    length: 1,
    convRule: rule23
  }, {
    start: 7814,
    length: 1,
    convRule: rule22
  }, {
    start: 7815,
    length: 1,
    convRule: rule23
  }, {
    start: 7816,
    length: 1,
    convRule: rule22
  }, {
    start: 7817,
    length: 1,
    convRule: rule23
  }, {
    start: 7818,
    length: 1,
    convRule: rule22
  }, {
    start: 7819,
    length: 1,
    convRule: rule23
  }, {
    start: 7820,
    length: 1,
    convRule: rule22
  }, {
    start: 7821,
    length: 1,
    convRule: rule23
  }, {
    start: 7822,
    length: 1,
    convRule: rule22
  }, {
    start: 7823,
    length: 1,
    convRule: rule23
  }, {
    start: 7824,
    length: 1,
    convRule: rule22
  }, {
    start: 7825,
    length: 1,
    convRule: rule23
  }, {
    start: 7826,
    length: 1,
    convRule: rule22
  }, {
    start: 7827,
    length: 1,
    convRule: rule23
  }, {
    start: 7828,
    length: 1,
    convRule: rule22
  }, {
    start: 7829,
    length: 1,
    convRule: rule23
  }, {
    start: 7830,
    length: 5,
    convRule: rule20
  }, {
    start: 7835,
    length: 1,
    convRule: rule141
  }, {
    start: 7836,
    length: 2,
    convRule: rule20
  }, {
    start: 7838,
    length: 1,
    convRule: rule142
  }, {
    start: 7839,
    length: 1,
    convRule: rule20
  }, {
    start: 7840,
    length: 1,
    convRule: rule22
  }, {
    start: 7841,
    length: 1,
    convRule: rule23
  }, {
    start: 7842,
    length: 1,
    convRule: rule22
  }, {
    start: 7843,
    length: 1,
    convRule: rule23
  }, {
    start: 7844,
    length: 1,
    convRule: rule22
  }, {
    start: 7845,
    length: 1,
    convRule: rule23
  }, {
    start: 7846,
    length: 1,
    convRule: rule22
  }, {
    start: 7847,
    length: 1,
    convRule: rule23
  }, {
    start: 7848,
    length: 1,
    convRule: rule22
  }, {
    start: 7849,
    length: 1,
    convRule: rule23
  }, {
    start: 7850,
    length: 1,
    convRule: rule22
  }, {
    start: 7851,
    length: 1,
    convRule: rule23
  }, {
    start: 7852,
    length: 1,
    convRule: rule22
  }, {
    start: 7853,
    length: 1,
    convRule: rule23
  }, {
    start: 7854,
    length: 1,
    convRule: rule22
  }, {
    start: 7855,
    length: 1,
    convRule: rule23
  }, {
    start: 7856,
    length: 1,
    convRule: rule22
  }, {
    start: 7857,
    length: 1,
    convRule: rule23
  }, {
    start: 7858,
    length: 1,
    convRule: rule22
  }, {
    start: 7859,
    length: 1,
    convRule: rule23
  }, {
    start: 7860,
    length: 1,
    convRule: rule22
  }, {
    start: 7861,
    length: 1,
    convRule: rule23
  }, {
    start: 7862,
    length: 1,
    convRule: rule22
  }, {
    start: 7863,
    length: 1,
    convRule: rule23
  }, {
    start: 7864,
    length: 1,
    convRule: rule22
  }, {
    start: 7865,
    length: 1,
    convRule: rule23
  }, {
    start: 7866,
    length: 1,
    convRule: rule22
  }, {
    start: 7867,
    length: 1,
    convRule: rule23
  }, {
    start: 7868,
    length: 1,
    convRule: rule22
  }, {
    start: 7869,
    length: 1,
    convRule: rule23
  }, {
    start: 7870,
    length: 1,
    convRule: rule22
  }, {
    start: 7871,
    length: 1,
    convRule: rule23
  }, {
    start: 7872,
    length: 1,
    convRule: rule22
  }, {
    start: 7873,
    length: 1,
    convRule: rule23
  }, {
    start: 7874,
    length: 1,
    convRule: rule22
  }, {
    start: 7875,
    length: 1,
    convRule: rule23
  }, {
    start: 7876,
    length: 1,
    convRule: rule22
  }, {
    start: 7877,
    length: 1,
    convRule: rule23
  }, {
    start: 7878,
    length: 1,
    convRule: rule22
  }, {
    start: 7879,
    length: 1,
    convRule: rule23
  }, {
    start: 7880,
    length: 1,
    convRule: rule22
  }, {
    start: 7881,
    length: 1,
    convRule: rule23
  }, {
    start: 7882,
    length: 1,
    convRule: rule22
  }, {
    start: 7883,
    length: 1,
    convRule: rule23
  }, {
    start: 7884,
    length: 1,
    convRule: rule22
  }, {
    start: 7885,
    length: 1,
    convRule: rule23
  }, {
    start: 7886,
    length: 1,
    convRule: rule22
  }, {
    start: 7887,
    length: 1,
    convRule: rule23
  }, {
    start: 7888,
    length: 1,
    convRule: rule22
  }, {
    start: 7889,
    length: 1,
    convRule: rule23
  }, {
    start: 7890,
    length: 1,
    convRule: rule22
  }, {
    start: 7891,
    length: 1,
    convRule: rule23
  }, {
    start: 7892,
    length: 1,
    convRule: rule22
  }, {
    start: 7893,
    length: 1,
    convRule: rule23
  }, {
    start: 7894,
    length: 1,
    convRule: rule22
  }, {
    start: 7895,
    length: 1,
    convRule: rule23
  }, {
    start: 7896,
    length: 1,
    convRule: rule22
  }, {
    start: 7897,
    length: 1,
    convRule: rule23
  }, {
    start: 7898,
    length: 1,
    convRule: rule22
  }, {
    start: 7899,
    length: 1,
    convRule: rule23
  }, {
    start: 7900,
    length: 1,
    convRule: rule22
  }, {
    start: 7901,
    length: 1,
    convRule: rule23
  }, {
    start: 7902,
    length: 1,
    convRule: rule22
  }, {
    start: 7903,
    length: 1,
    convRule: rule23
  }, {
    start: 7904,
    length: 1,
    convRule: rule22
  }, {
    start: 7905,
    length: 1,
    convRule: rule23
  }, {
    start: 7906,
    length: 1,
    convRule: rule22
  }, {
    start: 7907,
    length: 1,
    convRule: rule23
  }, {
    start: 7908,
    length: 1,
    convRule: rule22
  }, {
    start: 7909,
    length: 1,
    convRule: rule23
  }, {
    start: 7910,
    length: 1,
    convRule: rule22
  }, {
    start: 7911,
    length: 1,
    convRule: rule23
  }, {
    start: 7912,
    length: 1,
    convRule: rule22
  }, {
    start: 7913,
    length: 1,
    convRule: rule23
  }, {
    start: 7914,
    length: 1,
    convRule: rule22
  }, {
    start: 7915,
    length: 1,
    convRule: rule23
  }, {
    start: 7916,
    length: 1,
    convRule: rule22
  }, {
    start: 7917,
    length: 1,
    convRule: rule23
  }, {
    start: 7918,
    length: 1,
    convRule: rule22
  }, {
    start: 7919,
    length: 1,
    convRule: rule23
  }, {
    start: 7920,
    length: 1,
    convRule: rule22
  }, {
    start: 7921,
    length: 1,
    convRule: rule23
  }, {
    start: 7922,
    length: 1,
    convRule: rule22
  }, {
    start: 7923,
    length: 1,
    convRule: rule23
  }, {
    start: 7924,
    length: 1,
    convRule: rule22
  }, {
    start: 7925,
    length: 1,
    convRule: rule23
  }, {
    start: 7926,
    length: 1,
    convRule: rule22
  }, {
    start: 7927,
    length: 1,
    convRule: rule23
  }, {
    start: 7928,
    length: 1,
    convRule: rule22
  }, {
    start: 7929,
    length: 1,
    convRule: rule23
  }, {
    start: 7930,
    length: 1,
    convRule: rule22
  }, {
    start: 7931,
    length: 1,
    convRule: rule23
  }, {
    start: 7932,
    length: 1,
    convRule: rule22
  }, {
    start: 7933,
    length: 1,
    convRule: rule23
  }, {
    start: 7934,
    length: 1,
    convRule: rule22
  }, {
    start: 7935,
    length: 1,
    convRule: rule23
  }, {
    start: 7936,
    length: 8,
    convRule: rule143
  }, {
    start: 7944,
    length: 8,
    convRule: rule144
  }, {
    start: 7952,
    length: 6,
    convRule: rule143
  }, {
    start: 7960,
    length: 6,
    convRule: rule144
  }, {
    start: 7968,
    length: 8,
    convRule: rule143
  }, {
    start: 7976,
    length: 8,
    convRule: rule144
  }, {
    start: 7984,
    length: 8,
    convRule: rule143
  }, {
    start: 7992,
    length: 8,
    convRule: rule144
  }, {
    start: 8e3,
    length: 6,
    convRule: rule143
  }, {
    start: 8008,
    length: 6,
    convRule: rule144
  }, {
    start: 8016,
    length: 1,
    convRule: rule20
  }, {
    start: 8017,
    length: 1,
    convRule: rule143
  }, {
    start: 8018,
    length: 1,
    convRule: rule20
  }, {
    start: 8019,
    length: 1,
    convRule: rule143
  }, {
    start: 8020,
    length: 1,
    convRule: rule20
  }, {
    start: 8021,
    length: 1,
    convRule: rule143
  }, {
    start: 8022,
    length: 1,
    convRule: rule20
  }, {
    start: 8023,
    length: 1,
    convRule: rule143
  }, {
    start: 8025,
    length: 1,
    convRule: rule144
  }, {
    start: 8027,
    length: 1,
    convRule: rule144
  }, {
    start: 8029,
    length: 1,
    convRule: rule144
  }, {
    start: 8031,
    length: 1,
    convRule: rule144
  }, {
    start: 8032,
    length: 8,
    convRule: rule143
  }, {
    start: 8040,
    length: 8,
    convRule: rule144
  }, {
    start: 8048,
    length: 2,
    convRule: rule145
  }, {
    start: 8050,
    length: 4,
    convRule: rule146
  }, {
    start: 8054,
    length: 2,
    convRule: rule147
  }, {
    start: 8056,
    length: 2,
    convRule: rule148
  }, {
    start: 8058,
    length: 2,
    convRule: rule149
  }, {
    start: 8060,
    length: 2,
    convRule: rule150
  }, {
    start: 8064,
    length: 8,
    convRule: rule143
  }, {
    start: 8072,
    length: 8,
    convRule: rule151
  }, {
    start: 8080,
    length: 8,
    convRule: rule143
  }, {
    start: 8088,
    length: 8,
    convRule: rule151
  }, {
    start: 8096,
    length: 8,
    convRule: rule143
  }, {
    start: 8104,
    length: 8,
    convRule: rule151
  }, {
    start: 8112,
    length: 2,
    convRule: rule143
  }, {
    start: 8114,
    length: 1,
    convRule: rule20
  }, {
    start: 8115,
    length: 1,
    convRule: rule152
  }, {
    start: 8116,
    length: 1,
    convRule: rule20
  }, {
    start: 8118,
    length: 2,
    convRule: rule20
  }, {
    start: 8120,
    length: 2,
    convRule: rule144
  }, {
    start: 8122,
    length: 2,
    convRule: rule153
  }, {
    start: 8124,
    length: 1,
    convRule: rule154
  }, {
    start: 8125,
    length: 1,
    convRule: rule10
  }, {
    start: 8126,
    length: 1,
    convRule: rule155
  }, {
    start: 8127,
    length: 3,
    convRule: rule10
  }, {
    start: 8130,
    length: 1,
    convRule: rule20
  }, {
    start: 8131,
    length: 1,
    convRule: rule152
  }, {
    start: 8132,
    length: 1,
    convRule: rule20
  }, {
    start: 8134,
    length: 2,
    convRule: rule20
  }, {
    start: 8136,
    length: 4,
    convRule: rule156
  }, {
    start: 8140,
    length: 1,
    convRule: rule154
  }, {
    start: 8141,
    length: 3,
    convRule: rule10
  }, {
    start: 8144,
    length: 2,
    convRule: rule143
  }, {
    start: 8146,
    length: 2,
    convRule: rule20
  }, {
    start: 8150,
    length: 2,
    convRule: rule20
  }, {
    start: 8152,
    length: 2,
    convRule: rule144
  }, {
    start: 8154,
    length: 2,
    convRule: rule157
  }, {
    start: 8157,
    length: 3,
    convRule: rule10
  }, {
    start: 8160,
    length: 2,
    convRule: rule143
  }, {
    start: 8162,
    length: 3,
    convRule: rule20
  }, {
    start: 8165,
    length: 1,
    convRule: rule113
  }, {
    start: 8166,
    length: 2,
    convRule: rule20
  }, {
    start: 8168,
    length: 2,
    convRule: rule144
  }, {
    start: 8170,
    length: 2,
    convRule: rule158
  }, {
    start: 8172,
    length: 1,
    convRule: rule117
  }, {
    start: 8173,
    length: 3,
    convRule: rule10
  }, {
    start: 8178,
    length: 1,
    convRule: rule20
  }, {
    start: 8179,
    length: 1,
    convRule: rule152
  }, {
    start: 8180,
    length: 1,
    convRule: rule20
  }, {
    start: 8182,
    length: 2,
    convRule: rule20
  }, {
    start: 8184,
    length: 2,
    convRule: rule159
  }, {
    start: 8186,
    length: 2,
    convRule: rule160
  }, {
    start: 8188,
    length: 1,
    convRule: rule154
  }, {
    start: 8189,
    length: 2,
    convRule: rule10
  }, {
    start: 8192,
    length: 11,
    convRule: rule1
  }, {
    start: 8203,
    length: 5,
    convRule: rule16
  }, {
    start: 8208,
    length: 6,
    convRule: rule7
  }, {
    start: 8214,
    length: 2,
    convRule: rule2
  }, {
    start: 8216,
    length: 1,
    convRule: rule15
  }, {
    start: 8217,
    length: 1,
    convRule: rule19
  }, {
    start: 8218,
    length: 1,
    convRule: rule4
  }, {
    start: 8219,
    length: 2,
    convRule: rule15
  }, {
    start: 8221,
    length: 1,
    convRule: rule19
  }, {
    start: 8222,
    length: 1,
    convRule: rule4
  }, {
    start: 8223,
    length: 1,
    convRule: rule15
  }, {
    start: 8224,
    length: 8,
    convRule: rule2
  }, {
    start: 8232,
    length: 1,
    convRule: rule161
  }, {
    start: 8233,
    length: 1,
    convRule: rule162
  }, {
    start: 8234,
    length: 5,
    convRule: rule16
  }, {
    start: 8239,
    length: 1,
    convRule: rule1
  }, {
    start: 8240,
    length: 9,
    convRule: rule2
  }, {
    start: 8249,
    length: 1,
    convRule: rule15
  }, {
    start: 8250,
    length: 1,
    convRule: rule19
  }, {
    start: 8251,
    length: 4,
    convRule: rule2
  }, {
    start: 8255,
    length: 2,
    convRule: rule11
  }, {
    start: 8257,
    length: 3,
    convRule: rule2
  }, {
    start: 8260,
    length: 1,
    convRule: rule6
  }, {
    start: 8261,
    length: 1,
    convRule: rule4
  }, {
    start: 8262,
    length: 1,
    convRule: rule5
  }, {
    start: 8263,
    length: 11,
    convRule: rule2
  }, {
    start: 8274,
    length: 1,
    convRule: rule6
  }, {
    start: 8275,
    length: 1,
    convRule: rule2
  }, {
    start: 8276,
    length: 1,
    convRule: rule11
  }, {
    start: 8277,
    length: 10,
    convRule: rule2
  }, {
    start: 8287,
    length: 1,
    convRule: rule1
  }, {
    start: 8288,
    length: 5,
    convRule: rule16
  }, {
    start: 8294,
    length: 10,
    convRule: rule16
  }, {
    start: 8304,
    length: 1,
    convRule: rule17
  }, {
    start: 8305,
    length: 1,
    convRule: rule91
  }, {
    start: 8308,
    length: 6,
    convRule: rule17
  }, {
    start: 8314,
    length: 3,
    convRule: rule6
  }, {
    start: 8317,
    length: 1,
    convRule: rule4
  }, {
    start: 8318,
    length: 1,
    convRule: rule5
  }, {
    start: 8319,
    length: 1,
    convRule: rule91
  }, {
    start: 8320,
    length: 10,
    convRule: rule17
  }, {
    start: 8330,
    length: 3,
    convRule: rule6
  }, {
    start: 8333,
    length: 1,
    convRule: rule4
  }, {
    start: 8334,
    length: 1,
    convRule: rule5
  }, {
    start: 8336,
    length: 13,
    convRule: rule91
  }, {
    start: 8352,
    length: 32,
    convRule: rule3
  }, {
    start: 8400,
    length: 13,
    convRule: rule92
  }, {
    start: 8413,
    length: 4,
    convRule: rule119
  }, {
    start: 8417,
    length: 1,
    convRule: rule92
  }, {
    start: 8418,
    length: 3,
    convRule: rule119
  }, {
    start: 8421,
    length: 12,
    convRule: rule92
  }, {
    start: 8448,
    length: 2,
    convRule: rule13
  }, {
    start: 8450,
    length: 1,
    convRule: rule107
  }, {
    start: 8451,
    length: 4,
    convRule: rule13
  }, {
    start: 8455,
    length: 1,
    convRule: rule107
  }, {
    start: 8456,
    length: 2,
    convRule: rule13
  }, {
    start: 8458,
    length: 1,
    convRule: rule20
  }, {
    start: 8459,
    length: 3,
    convRule: rule107
  }, {
    start: 8462,
    length: 2,
    convRule: rule20
  }, {
    start: 8464,
    length: 3,
    convRule: rule107
  }, {
    start: 8467,
    length: 1,
    convRule: rule20
  }, {
    start: 8468,
    length: 1,
    convRule: rule13
  }, {
    start: 8469,
    length: 1,
    convRule: rule107
  }, {
    start: 8470,
    length: 2,
    convRule: rule13
  }, {
    start: 8472,
    length: 1,
    convRule: rule6
  }, {
    start: 8473,
    length: 5,
    convRule: rule107
  }, {
    start: 8478,
    length: 6,
    convRule: rule13
  }, {
    start: 8484,
    length: 1,
    convRule: rule107
  }, {
    start: 8485,
    length: 1,
    convRule: rule13
  }, {
    start: 8486,
    length: 1,
    convRule: rule163
  }, {
    start: 8487,
    length: 1,
    convRule: rule13
  }, {
    start: 8488,
    length: 1,
    convRule: rule107
  }, {
    start: 8489,
    length: 1,
    convRule: rule13
  }, {
    start: 8490,
    length: 1,
    convRule: rule164
  }, {
    start: 8491,
    length: 1,
    convRule: rule165
  }, {
    start: 8492,
    length: 2,
    convRule: rule107
  }, {
    start: 8494,
    length: 1,
    convRule: rule13
  }, {
    start: 8495,
    length: 1,
    convRule: rule20
  }, {
    start: 8496,
    length: 2,
    convRule: rule107
  }, {
    start: 8498,
    length: 1,
    convRule: rule166
  }, {
    start: 8499,
    length: 1,
    convRule: rule107
  }, {
    start: 8500,
    length: 1,
    convRule: rule20
  }, {
    start: 8501,
    length: 4,
    convRule: rule14
  }, {
    start: 8505,
    length: 1,
    convRule: rule20
  }, {
    start: 8506,
    length: 2,
    convRule: rule13
  }, {
    start: 8508,
    length: 2,
    convRule: rule20
  }, {
    start: 8510,
    length: 2,
    convRule: rule107
  }, {
    start: 8512,
    length: 5,
    convRule: rule6
  }, {
    start: 8517,
    length: 1,
    convRule: rule107
  }, {
    start: 8518,
    length: 4,
    convRule: rule20
  }, {
    start: 8522,
    length: 1,
    convRule: rule13
  }, {
    start: 8523,
    length: 1,
    convRule: rule6
  }, {
    start: 8524,
    length: 2,
    convRule: rule13
  }, {
    start: 8526,
    length: 1,
    convRule: rule167
  }, {
    start: 8527,
    length: 1,
    convRule: rule13
  }, {
    start: 8528,
    length: 16,
    convRule: rule17
  }, {
    start: 8544,
    length: 16,
    convRule: rule168
  }, {
    start: 8560,
    length: 16,
    convRule: rule169
  }, {
    start: 8576,
    length: 3,
    convRule: rule128
  }, {
    start: 8579,
    length: 1,
    convRule: rule22
  }, {
    start: 8580,
    length: 1,
    convRule: rule23
  }, {
    start: 8581,
    length: 4,
    convRule: rule128
  }, {
    start: 8585,
    length: 1,
    convRule: rule17
  }, {
    start: 8586,
    length: 2,
    convRule: rule13
  }, {
    start: 8592,
    length: 5,
    convRule: rule6
  }, {
    start: 8597,
    length: 5,
    convRule: rule13
  }, {
    start: 8602,
    length: 2,
    convRule: rule6
  }, {
    start: 8604,
    length: 4,
    convRule: rule13
  }, {
    start: 8608,
    length: 1,
    convRule: rule6
  }, {
    start: 8609,
    length: 2,
    convRule: rule13
  }, {
    start: 8611,
    length: 1,
    convRule: rule6
  }, {
    start: 8612,
    length: 2,
    convRule: rule13
  }, {
    start: 8614,
    length: 1,
    convRule: rule6
  }, {
    start: 8615,
    length: 7,
    convRule: rule13
  }, {
    start: 8622,
    length: 1,
    convRule: rule6
  }, {
    start: 8623,
    length: 31,
    convRule: rule13
  }, {
    start: 8654,
    length: 2,
    convRule: rule6
  }, {
    start: 8656,
    length: 2,
    convRule: rule13
  }, {
    start: 8658,
    length: 1,
    convRule: rule6
  }, {
    start: 8659,
    length: 1,
    convRule: rule13
  }, {
    start: 8660,
    length: 1,
    convRule: rule6
  }, {
    start: 8661,
    length: 31,
    convRule: rule13
  }, {
    start: 8692,
    length: 268,
    convRule: rule6
  }, {
    start: 8960,
    length: 8,
    convRule: rule13
  }, {
    start: 8968,
    length: 1,
    convRule: rule4
  }, {
    start: 8969,
    length: 1,
    convRule: rule5
  }, {
    start: 8970,
    length: 1,
    convRule: rule4
  }, {
    start: 8971,
    length: 1,
    convRule: rule5
  }, {
    start: 8972,
    length: 20,
    convRule: rule13
  }, {
    start: 8992,
    length: 2,
    convRule: rule6
  }, {
    start: 8994,
    length: 7,
    convRule: rule13
  }, {
    start: 9001,
    length: 1,
    convRule: rule4
  }, {
    start: 9002,
    length: 1,
    convRule: rule5
  }, {
    start: 9003,
    length: 81,
    convRule: rule13
  }, {
    start: 9084,
    length: 1,
    convRule: rule6
  }, {
    start: 9085,
    length: 30,
    convRule: rule13
  }, {
    start: 9115,
    length: 25,
    convRule: rule6
  }, {
    start: 9140,
    length: 40,
    convRule: rule13
  }, {
    start: 9180,
    length: 6,
    convRule: rule6
  }, {
    start: 9186,
    length: 69,
    convRule: rule13
  }, {
    start: 9280,
    length: 11,
    convRule: rule13
  }, {
    start: 9312,
    length: 60,
    convRule: rule17
  }, {
    start: 9372,
    length: 26,
    convRule: rule13
  }, {
    start: 9398,
    length: 26,
    convRule: rule170
  }, {
    start: 9424,
    length: 26,
    convRule: rule171
  }, {
    start: 9450,
    length: 22,
    convRule: rule17
  }, {
    start: 9472,
    length: 183,
    convRule: rule13
  }, {
    start: 9655,
    length: 1,
    convRule: rule6
  }, {
    start: 9656,
    length: 9,
    convRule: rule13
  }, {
    start: 9665,
    length: 1,
    convRule: rule6
  }, {
    start: 9666,
    length: 54,
    convRule: rule13
  }, {
    start: 9720,
    length: 8,
    convRule: rule6
  }, {
    start: 9728,
    length: 111,
    convRule: rule13
  }, {
    start: 9839,
    length: 1,
    convRule: rule6
  }, {
    start: 9840,
    length: 248,
    convRule: rule13
  }, {
    start: 10088,
    length: 1,
    convRule: rule4
  }, {
    start: 10089,
    length: 1,
    convRule: rule5
  }, {
    start: 10090,
    length: 1,
    convRule: rule4
  }, {
    start: 10091,
    length: 1,
    convRule: rule5
  }, {
    start: 10092,
    length: 1,
    convRule: rule4
  }, {
    start: 10093,
    length: 1,
    convRule: rule5
  }, {
    start: 10094,
    length: 1,
    convRule: rule4
  }, {
    start: 10095,
    length: 1,
    convRule: rule5
  }, {
    start: 10096,
    length: 1,
    convRule: rule4
  }, {
    start: 10097,
    length: 1,
    convRule: rule5
  }, {
    start: 10098,
    length: 1,
    convRule: rule4
  }, {
    start: 10099,
    length: 1,
    convRule: rule5
  }, {
    start: 10100,
    length: 1,
    convRule: rule4
  }, {
    start: 10101,
    length: 1,
    convRule: rule5
  }, {
    start: 10102,
    length: 30,
    convRule: rule17
  }, {
    start: 10132,
    length: 44,
    convRule: rule13
  }, {
    start: 10176,
    length: 5,
    convRule: rule6
  }, {
    start: 10181,
    length: 1,
    convRule: rule4
  }, {
    start: 10182,
    length: 1,
    convRule: rule5
  }, {
    start: 10183,
    length: 31,
    convRule: rule6
  }, {
    start: 10214,
    length: 1,
    convRule: rule4
  }, {
    start: 10215,
    length: 1,
    convRule: rule5
  }, {
    start: 10216,
    length: 1,
    convRule: rule4
  }, {
    start: 10217,
    length: 1,
    convRule: rule5
  }, {
    start: 10218,
    length: 1,
    convRule: rule4
  }, {
    start: 10219,
    length: 1,
    convRule: rule5
  }, {
    start: 10220,
    length: 1,
    convRule: rule4
  }, {
    start: 10221,
    length: 1,
    convRule: rule5
  }, {
    start: 10222,
    length: 1,
    convRule: rule4
  }, {
    start: 10223,
    length: 1,
    convRule: rule5
  }, {
    start: 10224,
    length: 16,
    convRule: rule6
  }, {
    start: 10240,
    length: 256,
    convRule: rule13
  }, {
    start: 10496,
    length: 131,
    convRule: rule6
  }, {
    start: 10627,
    length: 1,
    convRule: rule4
  }, {
    start: 10628,
    length: 1,
    convRule: rule5
  }, {
    start: 10629,
    length: 1,
    convRule: rule4
  }, {
    start: 10630,
    length: 1,
    convRule: rule5
  }, {
    start: 10631,
    length: 1,
    convRule: rule4
  }, {
    start: 10632,
    length: 1,
    convRule: rule5
  }, {
    start: 10633,
    length: 1,
    convRule: rule4
  }, {
    start: 10634,
    length: 1,
    convRule: rule5
  }, {
    start: 10635,
    length: 1,
    convRule: rule4
  }, {
    start: 10636,
    length: 1,
    convRule: rule5
  }, {
    start: 10637,
    length: 1,
    convRule: rule4
  }, {
    start: 10638,
    length: 1,
    convRule: rule5
  }, {
    start: 10639,
    length: 1,
    convRule: rule4
  }, {
    start: 10640,
    length: 1,
    convRule: rule5
  }, {
    start: 10641,
    length: 1,
    convRule: rule4
  }, {
    start: 10642,
    length: 1,
    convRule: rule5
  }, {
    start: 10643,
    length: 1,
    convRule: rule4
  }, {
    start: 10644,
    length: 1,
    convRule: rule5
  }, {
    start: 10645,
    length: 1,
    convRule: rule4
  }, {
    start: 10646,
    length: 1,
    convRule: rule5
  }, {
    start: 10647,
    length: 1,
    convRule: rule4
  }, {
    start: 10648,
    length: 1,
    convRule: rule5
  }, {
    start: 10649,
    length: 63,
    convRule: rule6
  }, {
    start: 10712,
    length: 1,
    convRule: rule4
  }, {
    start: 10713,
    length: 1,
    convRule: rule5
  }, {
    start: 10714,
    length: 1,
    convRule: rule4
  }, {
    start: 10715,
    length: 1,
    convRule: rule5
  }, {
    start: 10716,
    length: 32,
    convRule: rule6
  }, {
    start: 10748,
    length: 1,
    convRule: rule4
  }, {
    start: 10749,
    length: 1,
    convRule: rule5
  }, {
    start: 10750,
    length: 258,
    convRule: rule6
  }, {
    start: 11008,
    length: 48,
    convRule: rule13
  }, {
    start: 11056,
    length: 21,
    convRule: rule6
  }, {
    start: 11077,
    length: 2,
    convRule: rule13
  }, {
    start: 11079,
    length: 6,
    convRule: rule6
  }, {
    start: 11085,
    length: 39,
    convRule: rule13
  }, {
    start: 11126,
    length: 32,
    convRule: rule13
  }, {
    start: 11159,
    length: 105,
    convRule: rule13
  }, {
    start: 11264,
    length: 47,
    convRule: rule122
  }, {
    start: 11312,
    length: 47,
    convRule: rule123
  }, {
    start: 11360,
    length: 1,
    convRule: rule22
  }, {
    start: 11361,
    length: 1,
    convRule: rule23
  }, {
    start: 11362,
    length: 1,
    convRule: rule172
  }, {
    start: 11363,
    length: 1,
    convRule: rule173
  }, {
    start: 11364,
    length: 1,
    convRule: rule174
  }, {
    start: 11365,
    length: 1,
    convRule: rule175
  }, {
    start: 11366,
    length: 1,
    convRule: rule176
  }, {
    start: 11367,
    length: 1,
    convRule: rule22
  }, {
    start: 11368,
    length: 1,
    convRule: rule23
  }, {
    start: 11369,
    length: 1,
    convRule: rule22
  }, {
    start: 11370,
    length: 1,
    convRule: rule23
  }, {
    start: 11371,
    length: 1,
    convRule: rule22
  }, {
    start: 11372,
    length: 1,
    convRule: rule23
  }, {
    start: 11373,
    length: 1,
    convRule: rule177
  }, {
    start: 11374,
    length: 1,
    convRule: rule178
  }, {
    start: 11375,
    length: 1,
    convRule: rule179
  }, {
    start: 11376,
    length: 1,
    convRule: rule180
  }, {
    start: 11377,
    length: 1,
    convRule: rule20
  }, {
    start: 11378,
    length: 1,
    convRule: rule22
  }, {
    start: 11379,
    length: 1,
    convRule: rule23
  }, {
    start: 11380,
    length: 1,
    convRule: rule20
  }, {
    start: 11381,
    length: 1,
    convRule: rule22
  }, {
    start: 11382,
    length: 1,
    convRule: rule23
  }, {
    start: 11383,
    length: 5,
    convRule: rule20
  }, {
    start: 11388,
    length: 2,
    convRule: rule91
  }, {
    start: 11390,
    length: 2,
    convRule: rule181
  }, {
    start: 11392,
    length: 1,
    convRule: rule22
  }, {
    start: 11393,
    length: 1,
    convRule: rule23
  }, {
    start: 11394,
    length: 1,
    convRule: rule22
  }, {
    start: 11395,
    length: 1,
    convRule: rule23
  }, {
    start: 11396,
    length: 1,
    convRule: rule22
  }, {
    start: 11397,
    length: 1,
    convRule: rule23
  }, {
    start: 11398,
    length: 1,
    convRule: rule22
  }, {
    start: 11399,
    length: 1,
    convRule: rule23
  }, {
    start: 11400,
    length: 1,
    convRule: rule22
  }, {
    start: 11401,
    length: 1,
    convRule: rule23
  }, {
    start: 11402,
    length: 1,
    convRule: rule22
  }, {
    start: 11403,
    length: 1,
    convRule: rule23
  }, {
    start: 11404,
    length: 1,
    convRule: rule22
  }, {
    start: 11405,
    length: 1,
    convRule: rule23
  }, {
    start: 11406,
    length: 1,
    convRule: rule22
  }, {
    start: 11407,
    length: 1,
    convRule: rule23
  }, {
    start: 11408,
    length: 1,
    convRule: rule22
  }, {
    start: 11409,
    length: 1,
    convRule: rule23
  }, {
    start: 11410,
    length: 1,
    convRule: rule22
  }, {
    start: 11411,
    length: 1,
    convRule: rule23
  }, {
    start: 11412,
    length: 1,
    convRule: rule22
  }, {
    start: 11413,
    length: 1,
    convRule: rule23
  }, {
    start: 11414,
    length: 1,
    convRule: rule22
  }, {
    start: 11415,
    length: 1,
    convRule: rule23
  }, {
    start: 11416,
    length: 1,
    convRule: rule22
  }, {
    start: 11417,
    length: 1,
    convRule: rule23
  }, {
    start: 11418,
    length: 1,
    convRule: rule22
  }, {
    start: 11419,
    length: 1,
    convRule: rule23
  }, {
    start: 11420,
    length: 1,
    convRule: rule22
  }, {
    start: 11421,
    length: 1,
    convRule: rule23
  }, {
    start: 11422,
    length: 1,
    convRule: rule22
  }, {
    start: 11423,
    length: 1,
    convRule: rule23
  }, {
    start: 11424,
    length: 1,
    convRule: rule22
  }, {
    start: 11425,
    length: 1,
    convRule: rule23
  }, {
    start: 11426,
    length: 1,
    convRule: rule22
  }, {
    start: 11427,
    length: 1,
    convRule: rule23
  }, {
    start: 11428,
    length: 1,
    convRule: rule22
  }, {
    start: 11429,
    length: 1,
    convRule: rule23
  }, {
    start: 11430,
    length: 1,
    convRule: rule22
  }, {
    start: 11431,
    length: 1,
    convRule: rule23
  }, {
    start: 11432,
    length: 1,
    convRule: rule22
  }, {
    start: 11433,
    length: 1,
    convRule: rule23
  }, {
    start: 11434,
    length: 1,
    convRule: rule22
  }, {
    start: 11435,
    length: 1,
    convRule: rule23
  }, {
    start: 11436,
    length: 1,
    convRule: rule22
  }, {
    start: 11437,
    length: 1,
    convRule: rule23
  }, {
    start: 11438,
    length: 1,
    convRule: rule22
  }, {
    start: 11439,
    length: 1,
    convRule: rule23
  }, {
    start: 11440,
    length: 1,
    convRule: rule22
  }, {
    start: 11441,
    length: 1,
    convRule: rule23
  }, {
    start: 11442,
    length: 1,
    convRule: rule22
  }, {
    start: 11443,
    length: 1,
    convRule: rule23
  }, {
    start: 11444,
    length: 1,
    convRule: rule22
  }, {
    start: 11445,
    length: 1,
    convRule: rule23
  }, {
    start: 11446,
    length: 1,
    convRule: rule22
  }, {
    start: 11447,
    length: 1,
    convRule: rule23
  }, {
    start: 11448,
    length: 1,
    convRule: rule22
  }, {
    start: 11449,
    length: 1,
    convRule: rule23
  }, {
    start: 11450,
    length: 1,
    convRule: rule22
  }, {
    start: 11451,
    length: 1,
    convRule: rule23
  }, {
    start: 11452,
    length: 1,
    convRule: rule22
  }, {
    start: 11453,
    length: 1,
    convRule: rule23
  }, {
    start: 11454,
    length: 1,
    convRule: rule22
  }, {
    start: 11455,
    length: 1,
    convRule: rule23
  }, {
    start: 11456,
    length: 1,
    convRule: rule22
  }, {
    start: 11457,
    length: 1,
    convRule: rule23
  }, {
    start: 11458,
    length: 1,
    convRule: rule22
  }, {
    start: 11459,
    length: 1,
    convRule: rule23
  }, {
    start: 11460,
    length: 1,
    convRule: rule22
  }, {
    start: 11461,
    length: 1,
    convRule: rule23
  }, {
    start: 11462,
    length: 1,
    convRule: rule22
  }, {
    start: 11463,
    length: 1,
    convRule: rule23
  }, {
    start: 11464,
    length: 1,
    convRule: rule22
  }, {
    start: 11465,
    length: 1,
    convRule: rule23
  }, {
    start: 11466,
    length: 1,
    convRule: rule22
  }, {
    start: 11467,
    length: 1,
    convRule: rule23
  }, {
    start: 11468,
    length: 1,
    convRule: rule22
  }, {
    start: 11469,
    length: 1,
    convRule: rule23
  }, {
    start: 11470,
    length: 1,
    convRule: rule22
  }, {
    start: 11471,
    length: 1,
    convRule: rule23
  }, {
    start: 11472,
    length: 1,
    convRule: rule22
  }, {
    start: 11473,
    length: 1,
    convRule: rule23
  }, {
    start: 11474,
    length: 1,
    convRule: rule22
  }, {
    start: 11475,
    length: 1,
    convRule: rule23
  }, {
    start: 11476,
    length: 1,
    convRule: rule22
  }, {
    start: 11477,
    length: 1,
    convRule: rule23
  }, {
    start: 11478,
    length: 1,
    convRule: rule22
  }, {
    start: 11479,
    length: 1,
    convRule: rule23
  }, {
    start: 11480,
    length: 1,
    convRule: rule22
  }, {
    start: 11481,
    length: 1,
    convRule: rule23
  }, {
    start: 11482,
    length: 1,
    convRule: rule22
  }, {
    start: 11483,
    length: 1,
    convRule: rule23
  }, {
    start: 11484,
    length: 1,
    convRule: rule22
  }, {
    start: 11485,
    length: 1,
    convRule: rule23
  }, {
    start: 11486,
    length: 1,
    convRule: rule22
  }, {
    start: 11487,
    length: 1,
    convRule: rule23
  }, {
    start: 11488,
    length: 1,
    convRule: rule22
  }, {
    start: 11489,
    length: 1,
    convRule: rule23
  }, {
    start: 11490,
    length: 1,
    convRule: rule22
  }, {
    start: 11491,
    length: 1,
    convRule: rule23
  }, {
    start: 11492,
    length: 1,
    convRule: rule20
  }, {
    start: 11493,
    length: 6,
    convRule: rule13
  }, {
    start: 11499,
    length: 1,
    convRule: rule22
  }, {
    start: 11500,
    length: 1,
    convRule: rule23
  }, {
    start: 11501,
    length: 1,
    convRule: rule22
  }, {
    start: 11502,
    length: 1,
    convRule: rule23
  }, {
    start: 11503,
    length: 3,
    convRule: rule92
  }, {
    start: 11506,
    length: 1,
    convRule: rule22
  }, {
    start: 11507,
    length: 1,
    convRule: rule23
  }, {
    start: 11513,
    length: 4,
    convRule: rule2
  }, {
    start: 11517,
    length: 1,
    convRule: rule17
  }, {
    start: 11518,
    length: 2,
    convRule: rule2
  }, {
    start: 11520,
    length: 38,
    convRule: rule182
  }, {
    start: 11559,
    length: 1,
    convRule: rule182
  }, {
    start: 11565,
    length: 1,
    convRule: rule182
  }, {
    start: 11568,
    length: 56,
    convRule: rule14
  }, {
    start: 11631,
    length: 1,
    convRule: rule91
  }, {
    start: 11632,
    length: 1,
    convRule: rule2
  }, {
    start: 11647,
    length: 1,
    convRule: rule92
  }, {
    start: 11648,
    length: 23,
    convRule: rule14
  }, {
    start: 11680,
    length: 7,
    convRule: rule14
  }, {
    start: 11688,
    length: 7,
    convRule: rule14
  }, {
    start: 11696,
    length: 7,
    convRule: rule14
  }, {
    start: 11704,
    length: 7,
    convRule: rule14
  }, {
    start: 11712,
    length: 7,
    convRule: rule14
  }, {
    start: 11720,
    length: 7,
    convRule: rule14
  }, {
    start: 11728,
    length: 7,
    convRule: rule14
  }, {
    start: 11736,
    length: 7,
    convRule: rule14
  }, {
    start: 11744,
    length: 32,
    convRule: rule92
  }, {
    start: 11776,
    length: 2,
    convRule: rule2
  }, {
    start: 11778,
    length: 1,
    convRule: rule15
  }, {
    start: 11779,
    length: 1,
    convRule: rule19
  }, {
    start: 11780,
    length: 1,
    convRule: rule15
  }, {
    start: 11781,
    length: 1,
    convRule: rule19
  }, {
    start: 11782,
    length: 3,
    convRule: rule2
  }, {
    start: 11785,
    length: 1,
    convRule: rule15
  }, {
    start: 11786,
    length: 1,
    convRule: rule19
  }, {
    start: 11787,
    length: 1,
    convRule: rule2
  }, {
    start: 11788,
    length: 1,
    convRule: rule15
  }, {
    start: 11789,
    length: 1,
    convRule: rule19
  }, {
    start: 11790,
    length: 9,
    convRule: rule2
  }, {
    start: 11799,
    length: 1,
    convRule: rule7
  }, {
    start: 11800,
    length: 2,
    convRule: rule2
  }, {
    start: 11802,
    length: 1,
    convRule: rule7
  }, {
    start: 11803,
    length: 1,
    convRule: rule2
  }, {
    start: 11804,
    length: 1,
    convRule: rule15
  }, {
    start: 11805,
    length: 1,
    convRule: rule19
  }, {
    start: 11806,
    length: 2,
    convRule: rule2
  }, {
    start: 11808,
    length: 1,
    convRule: rule15
  }, {
    start: 11809,
    length: 1,
    convRule: rule19
  }, {
    start: 11810,
    length: 1,
    convRule: rule4
  }, {
    start: 11811,
    length: 1,
    convRule: rule5
  }, {
    start: 11812,
    length: 1,
    convRule: rule4
  }, {
    start: 11813,
    length: 1,
    convRule: rule5
  }, {
    start: 11814,
    length: 1,
    convRule: rule4
  }, {
    start: 11815,
    length: 1,
    convRule: rule5
  }, {
    start: 11816,
    length: 1,
    convRule: rule4
  }, {
    start: 11817,
    length: 1,
    convRule: rule5
  }, {
    start: 11818,
    length: 5,
    convRule: rule2
  }, {
    start: 11823,
    length: 1,
    convRule: rule91
  }, {
    start: 11824,
    length: 10,
    convRule: rule2
  }, {
    start: 11834,
    length: 2,
    convRule: rule7
  }, {
    start: 11836,
    length: 4,
    convRule: rule2
  }, {
    start: 11840,
    length: 1,
    convRule: rule7
  }, {
    start: 11841,
    length: 1,
    convRule: rule2
  }, {
    start: 11842,
    length: 1,
    convRule: rule4
  }, {
    start: 11843,
    length: 13,
    convRule: rule2
  }, {
    start: 11856,
    length: 2,
    convRule: rule13
  }, {
    start: 11858,
    length: 1,
    convRule: rule2
  }, {
    start: 11904,
    length: 26,
    convRule: rule13
  }, {
    start: 11931,
    length: 89,
    convRule: rule13
  }, {
    start: 12032,
    length: 214,
    convRule: rule13
  }, {
    start: 12272,
    length: 12,
    convRule: rule13
  }, {
    start: 12288,
    length: 1,
    convRule: rule1
  }, {
    start: 12289,
    length: 3,
    convRule: rule2
  }, {
    start: 12292,
    length: 1,
    convRule: rule13
  }, {
    start: 12293,
    length: 1,
    convRule: rule91
  }, {
    start: 12294,
    length: 1,
    convRule: rule14
  }, {
    start: 12295,
    length: 1,
    convRule: rule128
  }, {
    start: 12296,
    length: 1,
    convRule: rule4
  }, {
    start: 12297,
    length: 1,
    convRule: rule5
  }, {
    start: 12298,
    length: 1,
    convRule: rule4
  }, {
    start: 12299,
    length: 1,
    convRule: rule5
  }, {
    start: 12300,
    length: 1,
    convRule: rule4
  }, {
    start: 12301,
    length: 1,
    convRule: rule5
  }, {
    start: 12302,
    length: 1,
    convRule: rule4
  }, {
    start: 12303,
    length: 1,
    convRule: rule5
  }, {
    start: 12304,
    length: 1,
    convRule: rule4
  }, {
    start: 12305,
    length: 1,
    convRule: rule5
  }, {
    start: 12306,
    length: 2,
    convRule: rule13
  }, {
    start: 12308,
    length: 1,
    convRule: rule4
  }, {
    start: 12309,
    length: 1,
    convRule: rule5
  }, {
    start: 12310,
    length: 1,
    convRule: rule4
  }, {
    start: 12311,
    length: 1,
    convRule: rule5
  }, {
    start: 12312,
    length: 1,
    convRule: rule4
  }, {
    start: 12313,
    length: 1,
    convRule: rule5
  }, {
    start: 12314,
    length: 1,
    convRule: rule4
  }, {
    start: 12315,
    length: 1,
    convRule: rule5
  }, {
    start: 12316,
    length: 1,
    convRule: rule7
  }, {
    start: 12317,
    length: 1,
    convRule: rule4
  }, {
    start: 12318,
    length: 2,
    convRule: rule5
  }, {
    start: 12320,
    length: 1,
    convRule: rule13
  }, {
    start: 12321,
    length: 9,
    convRule: rule128
  }, {
    start: 12330,
    length: 4,
    convRule: rule92
  }, {
    start: 12334,
    length: 2,
    convRule: rule124
  }, {
    start: 12336,
    length: 1,
    convRule: rule7
  }, {
    start: 12337,
    length: 5,
    convRule: rule91
  }, {
    start: 12342,
    length: 2,
    convRule: rule13
  }, {
    start: 12344,
    length: 3,
    convRule: rule128
  }, {
    start: 12347,
    length: 1,
    convRule: rule91
  }, {
    start: 12348,
    length: 1,
    convRule: rule14
  }, {
    start: 12349,
    length: 1,
    convRule: rule2
  }, {
    start: 12350,
    length: 2,
    convRule: rule13
  }, {
    start: 12353,
    length: 86,
    convRule: rule14
  }, {
    start: 12441,
    length: 2,
    convRule: rule92
  }, {
    start: 12443,
    length: 2,
    convRule: rule10
  }, {
    start: 12445,
    length: 2,
    convRule: rule91
  }, {
    start: 12447,
    length: 1,
    convRule: rule14
  }, {
    start: 12448,
    length: 1,
    convRule: rule7
  }, {
    start: 12449,
    length: 90,
    convRule: rule14
  }, {
    start: 12539,
    length: 1,
    convRule: rule2
  }, {
    start: 12540,
    length: 3,
    convRule: rule91
  }, {
    start: 12543,
    length: 1,
    convRule: rule14
  }, {
    start: 12549,
    length: 43,
    convRule: rule14
  }, {
    start: 12593,
    length: 94,
    convRule: rule14
  }, {
    start: 12688,
    length: 2,
    convRule: rule13
  }, {
    start: 12690,
    length: 4,
    convRule: rule17
  }, {
    start: 12694,
    length: 10,
    convRule: rule13
  }, {
    start: 12704,
    length: 32,
    convRule: rule14
  }, {
    start: 12736,
    length: 36,
    convRule: rule13
  }, {
    start: 12784,
    length: 16,
    convRule: rule14
  }, {
    start: 12800,
    length: 31,
    convRule: rule13
  }, {
    start: 12832,
    length: 10,
    convRule: rule17
  }, {
    start: 12842,
    length: 30,
    convRule: rule13
  }, {
    start: 12872,
    length: 8,
    convRule: rule17
  }, {
    start: 12880,
    length: 1,
    convRule: rule13
  }, {
    start: 12881,
    length: 15,
    convRule: rule17
  }, {
    start: 12896,
    length: 32,
    convRule: rule13
  }, {
    start: 12928,
    length: 10,
    convRule: rule17
  }, {
    start: 12938,
    length: 39,
    convRule: rule13
  }, {
    start: 12977,
    length: 15,
    convRule: rule17
  }, {
    start: 12992,
    length: 320,
    convRule: rule13
  }, {
    start: 13312,
    length: 6592,
    convRule: rule14
  }, {
    start: 19904,
    length: 64,
    convRule: rule13
  }, {
    start: 19968,
    length: 20989,
    convRule: rule14
  }, {
    start: 40960,
    length: 21,
    convRule: rule14
  }, {
    start: 40981,
    length: 1,
    convRule: rule91
  }, {
    start: 40982,
    length: 1143,
    convRule: rule14
  }, {
    start: 42128,
    length: 55,
    convRule: rule13
  }, {
    start: 42192,
    length: 40,
    convRule: rule14
  }, {
    start: 42232,
    length: 6,
    convRule: rule91
  }, {
    start: 42238,
    length: 2,
    convRule: rule2
  }, {
    start: 42240,
    length: 268,
    convRule: rule14
  }, {
    start: 42508,
    length: 1,
    convRule: rule91
  }, {
    start: 42509,
    length: 3,
    convRule: rule2
  }, {
    start: 42512,
    length: 16,
    convRule: rule14
  }, {
    start: 42528,
    length: 10,
    convRule: rule8
  }, {
    start: 42538,
    length: 2,
    convRule: rule14
  }, {
    start: 42560,
    length: 1,
    convRule: rule22
  }, {
    start: 42561,
    length: 1,
    convRule: rule23
  }, {
    start: 42562,
    length: 1,
    convRule: rule22
  }, {
    start: 42563,
    length: 1,
    convRule: rule23
  }, {
    start: 42564,
    length: 1,
    convRule: rule22
  }, {
    start: 42565,
    length: 1,
    convRule: rule23
  }, {
    start: 42566,
    length: 1,
    convRule: rule22
  }, {
    start: 42567,
    length: 1,
    convRule: rule23
  }, {
    start: 42568,
    length: 1,
    convRule: rule22
  }, {
    start: 42569,
    length: 1,
    convRule: rule23
  }, {
    start: 42570,
    length: 1,
    convRule: rule22
  }, {
    start: 42571,
    length: 1,
    convRule: rule23
  }, {
    start: 42572,
    length: 1,
    convRule: rule22
  }, {
    start: 42573,
    length: 1,
    convRule: rule23
  }, {
    start: 42574,
    length: 1,
    convRule: rule22
  }, {
    start: 42575,
    length: 1,
    convRule: rule23
  }, {
    start: 42576,
    length: 1,
    convRule: rule22
  }, {
    start: 42577,
    length: 1,
    convRule: rule23
  }, {
    start: 42578,
    length: 1,
    convRule: rule22
  }, {
    start: 42579,
    length: 1,
    convRule: rule23
  }, {
    start: 42580,
    length: 1,
    convRule: rule22
  }, {
    start: 42581,
    length: 1,
    convRule: rule23
  }, {
    start: 42582,
    length: 1,
    convRule: rule22
  }, {
    start: 42583,
    length: 1,
    convRule: rule23
  }, {
    start: 42584,
    length: 1,
    convRule: rule22
  }, {
    start: 42585,
    length: 1,
    convRule: rule23
  }, {
    start: 42586,
    length: 1,
    convRule: rule22
  }, {
    start: 42587,
    length: 1,
    convRule: rule23
  }, {
    start: 42588,
    length: 1,
    convRule: rule22
  }, {
    start: 42589,
    length: 1,
    convRule: rule23
  }, {
    start: 42590,
    length: 1,
    convRule: rule22
  }, {
    start: 42591,
    length: 1,
    convRule: rule23
  }, {
    start: 42592,
    length: 1,
    convRule: rule22
  }, {
    start: 42593,
    length: 1,
    convRule: rule23
  }, {
    start: 42594,
    length: 1,
    convRule: rule22
  }, {
    start: 42595,
    length: 1,
    convRule: rule23
  }, {
    start: 42596,
    length: 1,
    convRule: rule22
  }, {
    start: 42597,
    length: 1,
    convRule: rule23
  }, {
    start: 42598,
    length: 1,
    convRule: rule22
  }, {
    start: 42599,
    length: 1,
    convRule: rule23
  }, {
    start: 42600,
    length: 1,
    convRule: rule22
  }, {
    start: 42601,
    length: 1,
    convRule: rule23
  }, {
    start: 42602,
    length: 1,
    convRule: rule22
  }, {
    start: 42603,
    length: 1,
    convRule: rule23
  }, {
    start: 42604,
    length: 1,
    convRule: rule22
  }, {
    start: 42605,
    length: 1,
    convRule: rule23
  }, {
    start: 42606,
    length: 1,
    convRule: rule14
  }, {
    start: 42607,
    length: 1,
    convRule: rule92
  }, {
    start: 42608,
    length: 3,
    convRule: rule119
  }, {
    start: 42611,
    length: 1,
    convRule: rule2
  }, {
    start: 42612,
    length: 10,
    convRule: rule92
  }, {
    start: 42622,
    length: 1,
    convRule: rule2
  }, {
    start: 42623,
    length: 1,
    convRule: rule91
  }, {
    start: 42624,
    length: 1,
    convRule: rule22
  }, {
    start: 42625,
    length: 1,
    convRule: rule23
  }, {
    start: 42626,
    length: 1,
    convRule: rule22
  }, {
    start: 42627,
    length: 1,
    convRule: rule23
  }, {
    start: 42628,
    length: 1,
    convRule: rule22
  }, {
    start: 42629,
    length: 1,
    convRule: rule23
  }, {
    start: 42630,
    length: 1,
    convRule: rule22
  }, {
    start: 42631,
    length: 1,
    convRule: rule23
  }, {
    start: 42632,
    length: 1,
    convRule: rule22
  }, {
    start: 42633,
    length: 1,
    convRule: rule23
  }, {
    start: 42634,
    length: 1,
    convRule: rule22
  }, {
    start: 42635,
    length: 1,
    convRule: rule23
  }, {
    start: 42636,
    length: 1,
    convRule: rule22
  }, {
    start: 42637,
    length: 1,
    convRule: rule23
  }, {
    start: 42638,
    length: 1,
    convRule: rule22
  }, {
    start: 42639,
    length: 1,
    convRule: rule23
  }, {
    start: 42640,
    length: 1,
    convRule: rule22
  }, {
    start: 42641,
    length: 1,
    convRule: rule23
  }, {
    start: 42642,
    length: 1,
    convRule: rule22
  }, {
    start: 42643,
    length: 1,
    convRule: rule23
  }, {
    start: 42644,
    length: 1,
    convRule: rule22
  }, {
    start: 42645,
    length: 1,
    convRule: rule23
  }, {
    start: 42646,
    length: 1,
    convRule: rule22
  }, {
    start: 42647,
    length: 1,
    convRule: rule23
  }, {
    start: 42648,
    length: 1,
    convRule: rule22
  }, {
    start: 42649,
    length: 1,
    convRule: rule23
  }, {
    start: 42650,
    length: 1,
    convRule: rule22
  }, {
    start: 42651,
    length: 1,
    convRule: rule23
  }, {
    start: 42652,
    length: 2,
    convRule: rule91
  }, {
    start: 42654,
    length: 2,
    convRule: rule92
  }, {
    start: 42656,
    length: 70,
    convRule: rule14
  }, {
    start: 42726,
    length: 10,
    convRule: rule128
  }, {
    start: 42736,
    length: 2,
    convRule: rule92
  }, {
    start: 42738,
    length: 6,
    convRule: rule2
  }, {
    start: 42752,
    length: 23,
    convRule: rule10
  }, {
    start: 42775,
    length: 9,
    convRule: rule91
  }, {
    start: 42784,
    length: 2,
    convRule: rule10
  }, {
    start: 42786,
    length: 1,
    convRule: rule22
  }, {
    start: 42787,
    length: 1,
    convRule: rule23
  }, {
    start: 42788,
    length: 1,
    convRule: rule22
  }, {
    start: 42789,
    length: 1,
    convRule: rule23
  }, {
    start: 42790,
    length: 1,
    convRule: rule22
  }, {
    start: 42791,
    length: 1,
    convRule: rule23
  }, {
    start: 42792,
    length: 1,
    convRule: rule22
  }, {
    start: 42793,
    length: 1,
    convRule: rule23
  }, {
    start: 42794,
    length: 1,
    convRule: rule22
  }, {
    start: 42795,
    length: 1,
    convRule: rule23
  }, {
    start: 42796,
    length: 1,
    convRule: rule22
  }, {
    start: 42797,
    length: 1,
    convRule: rule23
  }, {
    start: 42798,
    length: 1,
    convRule: rule22
  }, {
    start: 42799,
    length: 1,
    convRule: rule23
  }, {
    start: 42800,
    length: 2,
    convRule: rule20
  }, {
    start: 42802,
    length: 1,
    convRule: rule22
  }, {
    start: 42803,
    length: 1,
    convRule: rule23
  }, {
    start: 42804,
    length: 1,
    convRule: rule22
  }, {
    start: 42805,
    length: 1,
    convRule: rule23
  }, {
    start: 42806,
    length: 1,
    convRule: rule22
  }, {
    start: 42807,
    length: 1,
    convRule: rule23
  }, {
    start: 42808,
    length: 1,
    convRule: rule22
  }, {
    start: 42809,
    length: 1,
    convRule: rule23
  }, {
    start: 42810,
    length: 1,
    convRule: rule22
  }, {
    start: 42811,
    length: 1,
    convRule: rule23
  }, {
    start: 42812,
    length: 1,
    convRule: rule22
  }, {
    start: 42813,
    length: 1,
    convRule: rule23
  }, {
    start: 42814,
    length: 1,
    convRule: rule22
  }, {
    start: 42815,
    length: 1,
    convRule: rule23
  }, {
    start: 42816,
    length: 1,
    convRule: rule22
  }, {
    start: 42817,
    length: 1,
    convRule: rule23
  }, {
    start: 42818,
    length: 1,
    convRule: rule22
  }, {
    start: 42819,
    length: 1,
    convRule: rule23
  }, {
    start: 42820,
    length: 1,
    convRule: rule22
  }, {
    start: 42821,
    length: 1,
    convRule: rule23
  }, {
    start: 42822,
    length: 1,
    convRule: rule22
  }, {
    start: 42823,
    length: 1,
    convRule: rule23
  }, {
    start: 42824,
    length: 1,
    convRule: rule22
  }, {
    start: 42825,
    length: 1,
    convRule: rule23
  }, {
    start: 42826,
    length: 1,
    convRule: rule22
  }, {
    start: 42827,
    length: 1,
    convRule: rule23
  }, {
    start: 42828,
    length: 1,
    convRule: rule22
  }, {
    start: 42829,
    length: 1,
    convRule: rule23
  }, {
    start: 42830,
    length: 1,
    convRule: rule22
  }, {
    start: 42831,
    length: 1,
    convRule: rule23
  }, {
    start: 42832,
    length: 1,
    convRule: rule22
  }, {
    start: 42833,
    length: 1,
    convRule: rule23
  }, {
    start: 42834,
    length: 1,
    convRule: rule22
  }, {
    start: 42835,
    length: 1,
    convRule: rule23
  }, {
    start: 42836,
    length: 1,
    convRule: rule22
  }, {
    start: 42837,
    length: 1,
    convRule: rule23
  }, {
    start: 42838,
    length: 1,
    convRule: rule22
  }, {
    start: 42839,
    length: 1,
    convRule: rule23
  }, {
    start: 42840,
    length: 1,
    convRule: rule22
  }, {
    start: 42841,
    length: 1,
    convRule: rule23
  }, {
    start: 42842,
    length: 1,
    convRule: rule22
  }, {
    start: 42843,
    length: 1,
    convRule: rule23
  }, {
    start: 42844,
    length: 1,
    convRule: rule22
  }, {
    start: 42845,
    length: 1,
    convRule: rule23
  }, {
    start: 42846,
    length: 1,
    convRule: rule22
  }, {
    start: 42847,
    length: 1,
    convRule: rule23
  }, {
    start: 42848,
    length: 1,
    convRule: rule22
  }, {
    start: 42849,
    length: 1,
    convRule: rule23
  }, {
    start: 42850,
    length: 1,
    convRule: rule22
  }, {
    start: 42851,
    length: 1,
    convRule: rule23
  }, {
    start: 42852,
    length: 1,
    convRule: rule22
  }, {
    start: 42853,
    length: 1,
    convRule: rule23
  }, {
    start: 42854,
    length: 1,
    convRule: rule22
  }, {
    start: 42855,
    length: 1,
    convRule: rule23
  }, {
    start: 42856,
    length: 1,
    convRule: rule22
  }, {
    start: 42857,
    length: 1,
    convRule: rule23
  }, {
    start: 42858,
    length: 1,
    convRule: rule22
  }, {
    start: 42859,
    length: 1,
    convRule: rule23
  }, {
    start: 42860,
    length: 1,
    convRule: rule22
  }, {
    start: 42861,
    length: 1,
    convRule: rule23
  }, {
    start: 42862,
    length: 1,
    convRule: rule22
  }, {
    start: 42863,
    length: 1,
    convRule: rule23
  }, {
    start: 42864,
    length: 1,
    convRule: rule91
  }, {
    start: 42865,
    length: 8,
    convRule: rule20
  }, {
    start: 42873,
    length: 1,
    convRule: rule22
  }, {
    start: 42874,
    length: 1,
    convRule: rule23
  }, {
    start: 42875,
    length: 1,
    convRule: rule22
  }, {
    start: 42876,
    length: 1,
    convRule: rule23
  }, {
    start: 42877,
    length: 1,
    convRule: rule183
  }, {
    start: 42878,
    length: 1,
    convRule: rule22
  }, {
    start: 42879,
    length: 1,
    convRule: rule23
  }, {
    start: 42880,
    length: 1,
    convRule: rule22
  }, {
    start: 42881,
    length: 1,
    convRule: rule23
  }, {
    start: 42882,
    length: 1,
    convRule: rule22
  }, {
    start: 42883,
    length: 1,
    convRule: rule23
  }, {
    start: 42884,
    length: 1,
    convRule: rule22
  }, {
    start: 42885,
    length: 1,
    convRule: rule23
  }, {
    start: 42886,
    length: 1,
    convRule: rule22
  }, {
    start: 42887,
    length: 1,
    convRule: rule23
  }, {
    start: 42888,
    length: 1,
    convRule: rule91
  }, {
    start: 42889,
    length: 2,
    convRule: rule10
  }, {
    start: 42891,
    length: 1,
    convRule: rule22
  }, {
    start: 42892,
    length: 1,
    convRule: rule23
  }, {
    start: 42893,
    length: 1,
    convRule: rule184
  }, {
    start: 42894,
    length: 1,
    convRule: rule20
  }, {
    start: 42895,
    length: 1,
    convRule: rule14
  }, {
    start: 42896,
    length: 1,
    convRule: rule22
  }, {
    start: 42897,
    length: 1,
    convRule: rule23
  }, {
    start: 42898,
    length: 1,
    convRule: rule22
  }, {
    start: 42899,
    length: 1,
    convRule: rule23
  }, {
    start: 42900,
    length: 1,
    convRule: rule185
  }, {
    start: 42901,
    length: 1,
    convRule: rule20
  }, {
    start: 42902,
    length: 1,
    convRule: rule22
  }, {
    start: 42903,
    length: 1,
    convRule: rule23
  }, {
    start: 42904,
    length: 1,
    convRule: rule22
  }, {
    start: 42905,
    length: 1,
    convRule: rule23
  }, {
    start: 42906,
    length: 1,
    convRule: rule22
  }, {
    start: 42907,
    length: 1,
    convRule: rule23
  }, {
    start: 42908,
    length: 1,
    convRule: rule22
  }, {
    start: 42909,
    length: 1,
    convRule: rule23
  }, {
    start: 42910,
    length: 1,
    convRule: rule22
  }, {
    start: 42911,
    length: 1,
    convRule: rule23
  }, {
    start: 42912,
    length: 1,
    convRule: rule22
  }, {
    start: 42913,
    length: 1,
    convRule: rule23
  }, {
    start: 42914,
    length: 1,
    convRule: rule22
  }, {
    start: 42915,
    length: 1,
    convRule: rule23
  }, {
    start: 42916,
    length: 1,
    convRule: rule22
  }, {
    start: 42917,
    length: 1,
    convRule: rule23
  }, {
    start: 42918,
    length: 1,
    convRule: rule22
  }, {
    start: 42919,
    length: 1,
    convRule: rule23
  }, {
    start: 42920,
    length: 1,
    convRule: rule22
  }, {
    start: 42921,
    length: 1,
    convRule: rule23
  }, {
    start: 42922,
    length: 1,
    convRule: rule186
  }, {
    start: 42923,
    length: 1,
    convRule: rule187
  }, {
    start: 42924,
    length: 1,
    convRule: rule188
  }, {
    start: 42925,
    length: 1,
    convRule: rule189
  }, {
    start: 42926,
    length: 1,
    convRule: rule186
  }, {
    start: 42927,
    length: 1,
    convRule: rule20
  }, {
    start: 42928,
    length: 1,
    convRule: rule190
  }, {
    start: 42929,
    length: 1,
    convRule: rule191
  }, {
    start: 42930,
    length: 1,
    convRule: rule192
  }, {
    start: 42931,
    length: 1,
    convRule: rule193
  }, {
    start: 42932,
    length: 1,
    convRule: rule22
  }, {
    start: 42933,
    length: 1,
    convRule: rule23
  }, {
    start: 42934,
    length: 1,
    convRule: rule22
  }, {
    start: 42935,
    length: 1,
    convRule: rule23
  }, {
    start: 42936,
    length: 1,
    convRule: rule22
  }, {
    start: 42937,
    length: 1,
    convRule: rule23
  }, {
    start: 42938,
    length: 1,
    convRule: rule22
  }, {
    start: 42939,
    length: 1,
    convRule: rule23
  }, {
    start: 42940,
    length: 1,
    convRule: rule22
  }, {
    start: 42941,
    length: 1,
    convRule: rule23
  }, {
    start: 42942,
    length: 1,
    convRule: rule22
  }, {
    start: 42943,
    length: 1,
    convRule: rule23
  }, {
    start: 42946,
    length: 1,
    convRule: rule22
  }, {
    start: 42947,
    length: 1,
    convRule: rule23
  }, {
    start: 42948,
    length: 1,
    convRule: rule194
  }, {
    start: 42949,
    length: 1,
    convRule: rule195
  }, {
    start: 42950,
    length: 1,
    convRule: rule196
  }, {
    start: 42951,
    length: 1,
    convRule: rule22
  }, {
    start: 42952,
    length: 1,
    convRule: rule23
  }, {
    start: 42953,
    length: 1,
    convRule: rule22
  }, {
    start: 42954,
    length: 1,
    convRule: rule23
  }, {
    start: 42997,
    length: 1,
    convRule: rule22
  }, {
    start: 42998,
    length: 1,
    convRule: rule23
  }, {
    start: 42999,
    length: 1,
    convRule: rule14
  }, {
    start: 43e3,
    length: 2,
    convRule: rule91
  }, {
    start: 43002,
    length: 1,
    convRule: rule20
  }, {
    start: 43003,
    length: 7,
    convRule: rule14
  }, {
    start: 43010,
    length: 1,
    convRule: rule92
  }, {
    start: 43011,
    length: 3,
    convRule: rule14
  }, {
    start: 43014,
    length: 1,
    convRule: rule92
  }, {
    start: 43015,
    length: 4,
    convRule: rule14
  }, {
    start: 43019,
    length: 1,
    convRule: rule92
  }, {
    start: 43020,
    length: 23,
    convRule: rule14
  }, {
    start: 43043,
    length: 2,
    convRule: rule124
  }, {
    start: 43045,
    length: 2,
    convRule: rule92
  }, {
    start: 43047,
    length: 1,
    convRule: rule124
  }, {
    start: 43048,
    length: 4,
    convRule: rule13
  }, {
    start: 43052,
    length: 1,
    convRule: rule92
  }, {
    start: 43056,
    length: 6,
    convRule: rule17
  }, {
    start: 43062,
    length: 2,
    convRule: rule13
  }, {
    start: 43064,
    length: 1,
    convRule: rule3
  }, {
    start: 43065,
    length: 1,
    convRule: rule13
  }, {
    start: 43072,
    length: 52,
    convRule: rule14
  }, {
    start: 43124,
    length: 4,
    convRule: rule2
  }, {
    start: 43136,
    length: 2,
    convRule: rule124
  }, {
    start: 43138,
    length: 50,
    convRule: rule14
  }, {
    start: 43188,
    length: 16,
    convRule: rule124
  }, {
    start: 43204,
    length: 2,
    convRule: rule92
  }, {
    start: 43214,
    length: 2,
    convRule: rule2
  }, {
    start: 43216,
    length: 10,
    convRule: rule8
  }, {
    start: 43232,
    length: 18,
    convRule: rule92
  }, {
    start: 43250,
    length: 6,
    convRule: rule14
  }, {
    start: 43256,
    length: 3,
    convRule: rule2
  }, {
    start: 43259,
    length: 1,
    convRule: rule14
  }, {
    start: 43260,
    length: 1,
    convRule: rule2
  }, {
    start: 43261,
    length: 2,
    convRule: rule14
  }, {
    start: 43263,
    length: 1,
    convRule: rule92
  }, {
    start: 43264,
    length: 10,
    convRule: rule8
  }, {
    start: 43274,
    length: 28,
    convRule: rule14
  }, {
    start: 43302,
    length: 8,
    convRule: rule92
  }, {
    start: 43310,
    length: 2,
    convRule: rule2
  }, {
    start: 43312,
    length: 23,
    convRule: rule14
  }, {
    start: 43335,
    length: 11,
    convRule: rule92
  }, {
    start: 43346,
    length: 2,
    convRule: rule124
  }, {
    start: 43359,
    length: 1,
    convRule: rule2
  }, {
    start: 43360,
    length: 29,
    convRule: rule14
  }, {
    start: 43392,
    length: 3,
    convRule: rule92
  }, {
    start: 43395,
    length: 1,
    convRule: rule124
  }, {
    start: 43396,
    length: 47,
    convRule: rule14
  }, {
    start: 43443,
    length: 1,
    convRule: rule92
  }, {
    start: 43444,
    length: 2,
    convRule: rule124
  }, {
    start: 43446,
    length: 4,
    convRule: rule92
  }, {
    start: 43450,
    length: 2,
    convRule: rule124
  }, {
    start: 43452,
    length: 2,
    convRule: rule92
  }, {
    start: 43454,
    length: 3,
    convRule: rule124
  }, {
    start: 43457,
    length: 13,
    convRule: rule2
  }, {
    start: 43471,
    length: 1,
    convRule: rule91
  }, {
    start: 43472,
    length: 10,
    convRule: rule8
  }, {
    start: 43486,
    length: 2,
    convRule: rule2
  }, {
    start: 43488,
    length: 5,
    convRule: rule14
  }, {
    start: 43493,
    length: 1,
    convRule: rule92
  }, {
    start: 43494,
    length: 1,
    convRule: rule91
  }, {
    start: 43495,
    length: 9,
    convRule: rule14
  }, {
    start: 43504,
    length: 10,
    convRule: rule8
  }, {
    start: 43514,
    length: 5,
    convRule: rule14
  }, {
    start: 43520,
    length: 41,
    convRule: rule14
  }, {
    start: 43561,
    length: 6,
    convRule: rule92
  }, {
    start: 43567,
    length: 2,
    convRule: rule124
  }, {
    start: 43569,
    length: 2,
    convRule: rule92
  }, {
    start: 43571,
    length: 2,
    convRule: rule124
  }, {
    start: 43573,
    length: 2,
    convRule: rule92
  }, {
    start: 43584,
    length: 3,
    convRule: rule14
  }, {
    start: 43587,
    length: 1,
    convRule: rule92
  }, {
    start: 43588,
    length: 8,
    convRule: rule14
  }, {
    start: 43596,
    length: 1,
    convRule: rule92
  }, {
    start: 43597,
    length: 1,
    convRule: rule124
  }, {
    start: 43600,
    length: 10,
    convRule: rule8
  }, {
    start: 43612,
    length: 4,
    convRule: rule2
  }, {
    start: 43616,
    length: 16,
    convRule: rule14
  }, {
    start: 43632,
    length: 1,
    convRule: rule91
  }, {
    start: 43633,
    length: 6,
    convRule: rule14
  }, {
    start: 43639,
    length: 3,
    convRule: rule13
  }, {
    start: 43642,
    length: 1,
    convRule: rule14
  }, {
    start: 43643,
    length: 1,
    convRule: rule124
  }, {
    start: 43644,
    length: 1,
    convRule: rule92
  }, {
    start: 43645,
    length: 1,
    convRule: rule124
  }, {
    start: 43646,
    length: 50,
    convRule: rule14
  }, {
    start: 43696,
    length: 1,
    convRule: rule92
  }, {
    start: 43697,
    length: 1,
    convRule: rule14
  }, {
    start: 43698,
    length: 3,
    convRule: rule92
  }, {
    start: 43701,
    length: 2,
    convRule: rule14
  }, {
    start: 43703,
    length: 2,
    convRule: rule92
  }, {
    start: 43705,
    length: 5,
    convRule: rule14
  }, {
    start: 43710,
    length: 2,
    convRule: rule92
  }, {
    start: 43712,
    length: 1,
    convRule: rule14
  }, {
    start: 43713,
    length: 1,
    convRule: rule92
  }, {
    start: 43714,
    length: 1,
    convRule: rule14
  }, {
    start: 43739,
    length: 2,
    convRule: rule14
  }, {
    start: 43741,
    length: 1,
    convRule: rule91
  }, {
    start: 43742,
    length: 2,
    convRule: rule2
  }, {
    start: 43744,
    length: 11,
    convRule: rule14
  }, {
    start: 43755,
    length: 1,
    convRule: rule124
  }, {
    start: 43756,
    length: 2,
    convRule: rule92
  }, {
    start: 43758,
    length: 2,
    convRule: rule124
  }, {
    start: 43760,
    length: 2,
    convRule: rule2
  }, {
    start: 43762,
    length: 1,
    convRule: rule14
  }, {
    start: 43763,
    length: 2,
    convRule: rule91
  }, {
    start: 43765,
    length: 1,
    convRule: rule124
  }, {
    start: 43766,
    length: 1,
    convRule: rule92
  }, {
    start: 43777,
    length: 6,
    convRule: rule14
  }, {
    start: 43785,
    length: 6,
    convRule: rule14
  }, {
    start: 43793,
    length: 6,
    convRule: rule14
  }, {
    start: 43808,
    length: 7,
    convRule: rule14
  }, {
    start: 43816,
    length: 7,
    convRule: rule14
  }, {
    start: 43824,
    length: 35,
    convRule: rule20
  }, {
    start: 43859,
    length: 1,
    convRule: rule197
  }, {
    start: 43860,
    length: 7,
    convRule: rule20
  }, {
    start: 43867,
    length: 1,
    convRule: rule10
  }, {
    start: 43868,
    length: 4,
    convRule: rule91
  }, {
    start: 43872,
    length: 9,
    convRule: rule20
  }, {
    start: 43881,
    length: 1,
    convRule: rule91
  }, {
    start: 43882,
    length: 2,
    convRule: rule10
  }, {
    start: 43888,
    length: 80,
    convRule: rule198
  }, {
    start: 43968,
    length: 35,
    convRule: rule14
  }, {
    start: 44003,
    length: 2,
    convRule: rule124
  }, {
    start: 44005,
    length: 1,
    convRule: rule92
  }, {
    start: 44006,
    length: 2,
    convRule: rule124
  }, {
    start: 44008,
    length: 1,
    convRule: rule92
  }, {
    start: 44009,
    length: 2,
    convRule: rule124
  }, {
    start: 44011,
    length: 1,
    convRule: rule2
  }, {
    start: 44012,
    length: 1,
    convRule: rule124
  }, {
    start: 44013,
    length: 1,
    convRule: rule92
  }, {
    start: 44016,
    length: 10,
    convRule: rule8
  }, {
    start: 44032,
    length: 11172,
    convRule: rule14
  }, {
    start: 55216,
    length: 23,
    convRule: rule14
  }, {
    start: 55243,
    length: 49,
    convRule: rule14
  }, {
    start: 55296,
    length: 896,
    convRule: rule199
  }, {
    start: 56192,
    length: 128,
    convRule: rule199
  }, {
    start: 56320,
    length: 1024,
    convRule: rule199
  }, {
    start: 57344,
    length: 6400,
    convRule: rule200
  }, {
    start: 63744,
    length: 366,
    convRule: rule14
  }, {
    start: 64112,
    length: 106,
    convRule: rule14
  }, {
    start: 64256,
    length: 7,
    convRule: rule20
  }, {
    start: 64275,
    length: 5,
    convRule: rule20
  }, {
    start: 64285,
    length: 1,
    convRule: rule14
  }, {
    start: 64286,
    length: 1,
    convRule: rule92
  }, {
    start: 64287,
    length: 10,
    convRule: rule14
  }, {
    start: 64297,
    length: 1,
    convRule: rule6
  }, {
    start: 64298,
    length: 13,
    convRule: rule14
  }, {
    start: 64312,
    length: 5,
    convRule: rule14
  }, {
    start: 64318,
    length: 1,
    convRule: rule14
  }, {
    start: 64320,
    length: 2,
    convRule: rule14
  }, {
    start: 64323,
    length: 2,
    convRule: rule14
  }, {
    start: 64326,
    length: 108,
    convRule: rule14
  }, {
    start: 64434,
    length: 16,
    convRule: rule10
  }, {
    start: 64467,
    length: 363,
    convRule: rule14
  }, {
    start: 64830,
    length: 1,
    convRule: rule5
  }, {
    start: 64831,
    length: 1,
    convRule: rule4
  }, {
    start: 64848,
    length: 64,
    convRule: rule14
  }, {
    start: 64914,
    length: 54,
    convRule: rule14
  }, {
    start: 65008,
    length: 12,
    convRule: rule14
  }, {
    start: 65020,
    length: 1,
    convRule: rule3
  }, {
    start: 65021,
    length: 1,
    convRule: rule13
  }, {
    start: 65024,
    length: 16,
    convRule: rule92
  }, {
    start: 65040,
    length: 7,
    convRule: rule2
  }, {
    start: 65047,
    length: 1,
    convRule: rule4
  }, {
    start: 65048,
    length: 1,
    convRule: rule5
  }, {
    start: 65049,
    length: 1,
    convRule: rule2
  }, {
    start: 65056,
    length: 16,
    convRule: rule92
  }, {
    start: 65072,
    length: 1,
    convRule: rule2
  }, {
    start: 65073,
    length: 2,
    convRule: rule7
  }, {
    start: 65075,
    length: 2,
    convRule: rule11
  }, {
    start: 65077,
    length: 1,
    convRule: rule4
  }, {
    start: 65078,
    length: 1,
    convRule: rule5
  }, {
    start: 65079,
    length: 1,
    convRule: rule4
  }, {
    start: 65080,
    length: 1,
    convRule: rule5
  }, {
    start: 65081,
    length: 1,
    convRule: rule4
  }, {
    start: 65082,
    length: 1,
    convRule: rule5
  }, {
    start: 65083,
    length: 1,
    convRule: rule4
  }, {
    start: 65084,
    length: 1,
    convRule: rule5
  }, {
    start: 65085,
    length: 1,
    convRule: rule4
  }, {
    start: 65086,
    length: 1,
    convRule: rule5
  }, {
    start: 65087,
    length: 1,
    convRule: rule4
  }, {
    start: 65088,
    length: 1,
    convRule: rule5
  }, {
    start: 65089,
    length: 1,
    convRule: rule4
  }, {
    start: 65090,
    length: 1,
    convRule: rule5
  }, {
    start: 65091,
    length: 1,
    convRule: rule4
  }, {
    start: 65092,
    length: 1,
    convRule: rule5
  }, {
    start: 65093,
    length: 2,
    convRule: rule2
  }, {
    start: 65095,
    length: 1,
    convRule: rule4
  }, {
    start: 65096,
    length: 1,
    convRule: rule5
  }, {
    start: 65097,
    length: 4,
    convRule: rule2
  }, {
    start: 65101,
    length: 3,
    convRule: rule11
  }, {
    start: 65104,
    length: 3,
    convRule: rule2
  }, {
    start: 65108,
    length: 4,
    convRule: rule2
  }, {
    start: 65112,
    length: 1,
    convRule: rule7
  }, {
    start: 65113,
    length: 1,
    convRule: rule4
  }, {
    start: 65114,
    length: 1,
    convRule: rule5
  }, {
    start: 65115,
    length: 1,
    convRule: rule4
  }, {
    start: 65116,
    length: 1,
    convRule: rule5
  }, {
    start: 65117,
    length: 1,
    convRule: rule4
  }, {
    start: 65118,
    length: 1,
    convRule: rule5
  }, {
    start: 65119,
    length: 3,
    convRule: rule2
  }, {
    start: 65122,
    length: 1,
    convRule: rule6
  }, {
    start: 65123,
    length: 1,
    convRule: rule7
  }, {
    start: 65124,
    length: 3,
    convRule: rule6
  }, {
    start: 65128,
    length: 1,
    convRule: rule2
  }, {
    start: 65129,
    length: 1,
    convRule: rule3
  }, {
    start: 65130,
    length: 2,
    convRule: rule2
  }, {
    start: 65136,
    length: 5,
    convRule: rule14
  }, {
    start: 65142,
    length: 135,
    convRule: rule14
  }, {
    start: 65279,
    length: 1,
    convRule: rule16
  }, {
    start: 65281,
    length: 3,
    convRule: rule2
  }, {
    start: 65284,
    length: 1,
    convRule: rule3
  }, {
    start: 65285,
    length: 3,
    convRule: rule2
  }, {
    start: 65288,
    length: 1,
    convRule: rule4
  }, {
    start: 65289,
    length: 1,
    convRule: rule5
  }, {
    start: 65290,
    length: 1,
    convRule: rule2
  }, {
    start: 65291,
    length: 1,
    convRule: rule6
  }, {
    start: 65292,
    length: 1,
    convRule: rule2
  }, {
    start: 65293,
    length: 1,
    convRule: rule7
  }, {
    start: 65294,
    length: 2,
    convRule: rule2
  }, {
    start: 65296,
    length: 10,
    convRule: rule8
  }, {
    start: 65306,
    length: 2,
    convRule: rule2
  }, {
    start: 65308,
    length: 3,
    convRule: rule6
  }, {
    start: 65311,
    length: 2,
    convRule: rule2
  }, {
    start: 65313,
    length: 26,
    convRule: rule9
  }, {
    start: 65339,
    length: 1,
    convRule: rule4
  }, {
    start: 65340,
    length: 1,
    convRule: rule2
  }, {
    start: 65341,
    length: 1,
    convRule: rule5
  }, {
    start: 65342,
    length: 1,
    convRule: rule10
  }, {
    start: 65343,
    length: 1,
    convRule: rule11
  }, {
    start: 65344,
    length: 1,
    convRule: rule10
  }, {
    start: 65345,
    length: 26,
    convRule: rule12
  }, {
    start: 65371,
    length: 1,
    convRule: rule4
  }, {
    start: 65372,
    length: 1,
    convRule: rule6
  }, {
    start: 65373,
    length: 1,
    convRule: rule5
  }, {
    start: 65374,
    length: 1,
    convRule: rule6
  }, {
    start: 65375,
    length: 1,
    convRule: rule4
  }, {
    start: 65376,
    length: 1,
    convRule: rule5
  }, {
    start: 65377,
    length: 1,
    convRule: rule2
  }, {
    start: 65378,
    length: 1,
    convRule: rule4
  }, {
    start: 65379,
    length: 1,
    convRule: rule5
  }, {
    start: 65380,
    length: 2,
    convRule: rule2
  }, {
    start: 65382,
    length: 10,
    convRule: rule14
  }, {
    start: 65392,
    length: 1,
    convRule: rule91
  }, {
    start: 65393,
    length: 45,
    convRule: rule14
  }, {
    start: 65438,
    length: 2,
    convRule: rule91
  }, {
    start: 65440,
    length: 31,
    convRule: rule14
  }, {
    start: 65474,
    length: 6,
    convRule: rule14
  }, {
    start: 65482,
    length: 6,
    convRule: rule14
  }, {
    start: 65490,
    length: 6,
    convRule: rule14
  }, {
    start: 65498,
    length: 3,
    convRule: rule14
  }, {
    start: 65504,
    length: 2,
    convRule: rule3
  }, {
    start: 65506,
    length: 1,
    convRule: rule6
  }, {
    start: 65507,
    length: 1,
    convRule: rule10
  }, {
    start: 65508,
    length: 1,
    convRule: rule13
  }, {
    start: 65509,
    length: 2,
    convRule: rule3
  }, {
    start: 65512,
    length: 1,
    convRule: rule13
  }, {
    start: 65513,
    length: 4,
    convRule: rule6
  }, {
    start: 65517,
    length: 2,
    convRule: rule13
  }, {
    start: 65529,
    length: 3,
    convRule: rule16
  }, {
    start: 65532,
    length: 2,
    convRule: rule13
  }, {
    start: 65536,
    length: 12,
    convRule: rule14
  }, {
    start: 65549,
    length: 26,
    convRule: rule14
  }, {
    start: 65576,
    length: 19,
    convRule: rule14
  }, {
    start: 65596,
    length: 2,
    convRule: rule14
  }, {
    start: 65599,
    length: 15,
    convRule: rule14
  }, {
    start: 65616,
    length: 14,
    convRule: rule14
  }, {
    start: 65664,
    length: 123,
    convRule: rule14
  }, {
    start: 65792,
    length: 3,
    convRule: rule2
  }, {
    start: 65799,
    length: 45,
    convRule: rule17
  }, {
    start: 65847,
    length: 9,
    convRule: rule13
  }, {
    start: 65856,
    length: 53,
    convRule: rule128
  }, {
    start: 65909,
    length: 4,
    convRule: rule17
  }, {
    start: 65913,
    length: 17,
    convRule: rule13
  }, {
    start: 65930,
    length: 2,
    convRule: rule17
  }, {
    start: 65932,
    length: 3,
    convRule: rule13
  }, {
    start: 65936,
    length: 13,
    convRule: rule13
  }, {
    start: 65952,
    length: 1,
    convRule: rule13
  }, {
    start: 66e3,
    length: 45,
    convRule: rule13
  }, {
    start: 66045,
    length: 1,
    convRule: rule92
  }, {
    start: 66176,
    length: 29,
    convRule: rule14
  }, {
    start: 66208,
    length: 49,
    convRule: rule14
  }, {
    start: 66272,
    length: 1,
    convRule: rule92
  }, {
    start: 66273,
    length: 27,
    convRule: rule17
  }, {
    start: 66304,
    length: 32,
    convRule: rule14
  }, {
    start: 66336,
    length: 4,
    convRule: rule17
  }, {
    start: 66349,
    length: 20,
    convRule: rule14
  }, {
    start: 66369,
    length: 1,
    convRule: rule128
  }, {
    start: 66370,
    length: 8,
    convRule: rule14
  }, {
    start: 66378,
    length: 1,
    convRule: rule128
  }, {
    start: 66384,
    length: 38,
    convRule: rule14
  }, {
    start: 66422,
    length: 5,
    convRule: rule92
  }, {
    start: 66432,
    length: 30,
    convRule: rule14
  }, {
    start: 66463,
    length: 1,
    convRule: rule2
  }, {
    start: 66464,
    length: 36,
    convRule: rule14
  }, {
    start: 66504,
    length: 8,
    convRule: rule14
  }, {
    start: 66512,
    length: 1,
    convRule: rule2
  }, {
    start: 66513,
    length: 5,
    convRule: rule128
  }, {
    start: 66560,
    length: 40,
    convRule: rule201
  }, {
    start: 66600,
    length: 40,
    convRule: rule202
  }, {
    start: 66640,
    length: 78,
    convRule: rule14
  }, {
    start: 66720,
    length: 10,
    convRule: rule8
  }, {
    start: 66736,
    length: 36,
    convRule: rule201
  }, {
    start: 66776,
    length: 36,
    convRule: rule202
  }, {
    start: 66816,
    length: 40,
    convRule: rule14
  }, {
    start: 66864,
    length: 52,
    convRule: rule14
  }, {
    start: 66927,
    length: 1,
    convRule: rule2
  }, {
    start: 67072,
    length: 311,
    convRule: rule14
  }, {
    start: 67392,
    length: 22,
    convRule: rule14
  }, {
    start: 67424,
    length: 8,
    convRule: rule14
  }, {
    start: 67584,
    length: 6,
    convRule: rule14
  }, {
    start: 67592,
    length: 1,
    convRule: rule14
  }, {
    start: 67594,
    length: 44,
    convRule: rule14
  }, {
    start: 67639,
    length: 2,
    convRule: rule14
  }, {
    start: 67644,
    length: 1,
    convRule: rule14
  }, {
    start: 67647,
    length: 23,
    convRule: rule14
  }, {
    start: 67671,
    length: 1,
    convRule: rule2
  }, {
    start: 67672,
    length: 8,
    convRule: rule17
  }, {
    start: 67680,
    length: 23,
    convRule: rule14
  }, {
    start: 67703,
    length: 2,
    convRule: rule13
  }, {
    start: 67705,
    length: 7,
    convRule: rule17
  }, {
    start: 67712,
    length: 31,
    convRule: rule14
  }, {
    start: 67751,
    length: 9,
    convRule: rule17
  }, {
    start: 67808,
    length: 19,
    convRule: rule14
  }, {
    start: 67828,
    length: 2,
    convRule: rule14
  }, {
    start: 67835,
    length: 5,
    convRule: rule17
  }, {
    start: 67840,
    length: 22,
    convRule: rule14
  }, {
    start: 67862,
    length: 6,
    convRule: rule17
  }, {
    start: 67871,
    length: 1,
    convRule: rule2
  }, {
    start: 67872,
    length: 26,
    convRule: rule14
  }, {
    start: 67903,
    length: 1,
    convRule: rule2
  }, {
    start: 67968,
    length: 56,
    convRule: rule14
  }, {
    start: 68028,
    length: 2,
    convRule: rule17
  }, {
    start: 68030,
    length: 2,
    convRule: rule14
  }, {
    start: 68032,
    length: 16,
    convRule: rule17
  }, {
    start: 68050,
    length: 46,
    convRule: rule17
  }, {
    start: 68096,
    length: 1,
    convRule: rule14
  }, {
    start: 68097,
    length: 3,
    convRule: rule92
  }, {
    start: 68101,
    length: 2,
    convRule: rule92
  }, {
    start: 68108,
    length: 4,
    convRule: rule92
  }, {
    start: 68112,
    length: 4,
    convRule: rule14
  }, {
    start: 68117,
    length: 3,
    convRule: rule14
  }, {
    start: 68121,
    length: 29,
    convRule: rule14
  }, {
    start: 68152,
    length: 3,
    convRule: rule92
  }, {
    start: 68159,
    length: 1,
    convRule: rule92
  }, {
    start: 68160,
    length: 9,
    convRule: rule17
  }, {
    start: 68176,
    length: 9,
    convRule: rule2
  }, {
    start: 68192,
    length: 29,
    convRule: rule14
  }, {
    start: 68221,
    length: 2,
    convRule: rule17
  }, {
    start: 68223,
    length: 1,
    convRule: rule2
  }, {
    start: 68224,
    length: 29,
    convRule: rule14
  }, {
    start: 68253,
    length: 3,
    convRule: rule17
  }, {
    start: 68288,
    length: 8,
    convRule: rule14
  }, {
    start: 68296,
    length: 1,
    convRule: rule13
  }, {
    start: 68297,
    length: 28,
    convRule: rule14
  }, {
    start: 68325,
    length: 2,
    convRule: rule92
  }, {
    start: 68331,
    length: 5,
    convRule: rule17
  }, {
    start: 68336,
    length: 7,
    convRule: rule2
  }, {
    start: 68352,
    length: 54,
    convRule: rule14
  }, {
    start: 68409,
    length: 7,
    convRule: rule2
  }, {
    start: 68416,
    length: 22,
    convRule: rule14
  }, {
    start: 68440,
    length: 8,
    convRule: rule17
  }, {
    start: 68448,
    length: 19,
    convRule: rule14
  }, {
    start: 68472,
    length: 8,
    convRule: rule17
  }, {
    start: 68480,
    length: 18,
    convRule: rule14
  }, {
    start: 68505,
    length: 4,
    convRule: rule2
  }, {
    start: 68521,
    length: 7,
    convRule: rule17
  }, {
    start: 68608,
    length: 73,
    convRule: rule14
  }, {
    start: 68736,
    length: 51,
    convRule: rule97
  }, {
    start: 68800,
    length: 51,
    convRule: rule102
  }, {
    start: 68858,
    length: 6,
    convRule: rule17
  }, {
    start: 68864,
    length: 36,
    convRule: rule14
  }, {
    start: 68900,
    length: 4,
    convRule: rule92
  }, {
    start: 68912,
    length: 10,
    convRule: rule8
  }, {
    start: 69216,
    length: 31,
    convRule: rule17
  }, {
    start: 69248,
    length: 42,
    convRule: rule14
  }, {
    start: 69291,
    length: 2,
    convRule: rule92
  }, {
    start: 69293,
    length: 1,
    convRule: rule7
  }, {
    start: 69296,
    length: 2,
    convRule: rule14
  }, {
    start: 69376,
    length: 29,
    convRule: rule14
  }, {
    start: 69405,
    length: 10,
    convRule: rule17
  }, {
    start: 69415,
    length: 1,
    convRule: rule14
  }, {
    start: 69424,
    length: 22,
    convRule: rule14
  }, {
    start: 69446,
    length: 11,
    convRule: rule92
  }, {
    start: 69457,
    length: 4,
    convRule: rule17
  }, {
    start: 69461,
    length: 5,
    convRule: rule2
  }, {
    start: 69552,
    length: 21,
    convRule: rule14
  }, {
    start: 69573,
    length: 7,
    convRule: rule17
  }, {
    start: 69600,
    length: 23,
    convRule: rule14
  }, {
    start: 69632,
    length: 1,
    convRule: rule124
  }, {
    start: 69633,
    length: 1,
    convRule: rule92
  }, {
    start: 69634,
    length: 1,
    convRule: rule124
  }, {
    start: 69635,
    length: 53,
    convRule: rule14
  }, {
    start: 69688,
    length: 15,
    convRule: rule92
  }, {
    start: 69703,
    length: 7,
    convRule: rule2
  }, {
    start: 69714,
    length: 20,
    convRule: rule17
  }, {
    start: 69734,
    length: 10,
    convRule: rule8
  }, {
    start: 69759,
    length: 3,
    convRule: rule92
  }, {
    start: 69762,
    length: 1,
    convRule: rule124
  }, {
    start: 69763,
    length: 45,
    convRule: rule14
  }, {
    start: 69808,
    length: 3,
    convRule: rule124
  }, {
    start: 69811,
    length: 4,
    convRule: rule92
  }, {
    start: 69815,
    length: 2,
    convRule: rule124
  }, {
    start: 69817,
    length: 2,
    convRule: rule92
  }, {
    start: 69819,
    length: 2,
    convRule: rule2
  }, {
    start: 69821,
    length: 1,
    convRule: rule16
  }, {
    start: 69822,
    length: 4,
    convRule: rule2
  }, {
    start: 69837,
    length: 1,
    convRule: rule16
  }, {
    start: 69840,
    length: 25,
    convRule: rule14
  }, {
    start: 69872,
    length: 10,
    convRule: rule8
  }, {
    start: 69888,
    length: 3,
    convRule: rule92
  }, {
    start: 69891,
    length: 36,
    convRule: rule14
  }, {
    start: 69927,
    length: 5,
    convRule: rule92
  }, {
    start: 69932,
    length: 1,
    convRule: rule124
  }, {
    start: 69933,
    length: 8,
    convRule: rule92
  }, {
    start: 69942,
    length: 10,
    convRule: rule8
  }, {
    start: 69952,
    length: 4,
    convRule: rule2
  }, {
    start: 69956,
    length: 1,
    convRule: rule14
  }, {
    start: 69957,
    length: 2,
    convRule: rule124
  }, {
    start: 69959,
    length: 1,
    convRule: rule14
  }, {
    start: 69968,
    length: 35,
    convRule: rule14
  }, {
    start: 70003,
    length: 1,
    convRule: rule92
  }, {
    start: 70004,
    length: 2,
    convRule: rule2
  }, {
    start: 70006,
    length: 1,
    convRule: rule14
  }, {
    start: 70016,
    length: 2,
    convRule: rule92
  }, {
    start: 70018,
    length: 1,
    convRule: rule124
  }, {
    start: 70019,
    length: 48,
    convRule: rule14
  }, {
    start: 70067,
    length: 3,
    convRule: rule124
  }, {
    start: 70070,
    length: 9,
    convRule: rule92
  }, {
    start: 70079,
    length: 2,
    convRule: rule124
  }, {
    start: 70081,
    length: 4,
    convRule: rule14
  }, {
    start: 70085,
    length: 4,
    convRule: rule2
  }, {
    start: 70089,
    length: 4,
    convRule: rule92
  }, {
    start: 70093,
    length: 1,
    convRule: rule2
  }, {
    start: 70094,
    length: 1,
    convRule: rule124
  }, {
    start: 70095,
    length: 1,
    convRule: rule92
  }, {
    start: 70096,
    length: 10,
    convRule: rule8
  }, {
    start: 70106,
    length: 1,
    convRule: rule14
  }, {
    start: 70107,
    length: 1,
    convRule: rule2
  }, {
    start: 70108,
    length: 1,
    convRule: rule14
  }, {
    start: 70109,
    length: 3,
    convRule: rule2
  }, {
    start: 70113,
    length: 20,
    convRule: rule17
  }, {
    start: 70144,
    length: 18,
    convRule: rule14
  }, {
    start: 70163,
    length: 25,
    convRule: rule14
  }, {
    start: 70188,
    length: 3,
    convRule: rule124
  }, {
    start: 70191,
    length: 3,
    convRule: rule92
  }, {
    start: 70194,
    length: 2,
    convRule: rule124
  }, {
    start: 70196,
    length: 1,
    convRule: rule92
  }, {
    start: 70197,
    length: 1,
    convRule: rule124
  }, {
    start: 70198,
    length: 2,
    convRule: rule92
  }, {
    start: 70200,
    length: 6,
    convRule: rule2
  }, {
    start: 70206,
    length: 1,
    convRule: rule92
  }, {
    start: 70272,
    length: 7,
    convRule: rule14
  }, {
    start: 70280,
    length: 1,
    convRule: rule14
  }, {
    start: 70282,
    length: 4,
    convRule: rule14
  }, {
    start: 70287,
    length: 15,
    convRule: rule14
  }, {
    start: 70303,
    length: 10,
    convRule: rule14
  }, {
    start: 70313,
    length: 1,
    convRule: rule2
  }, {
    start: 70320,
    length: 47,
    convRule: rule14
  }, {
    start: 70367,
    length: 1,
    convRule: rule92
  }, {
    start: 70368,
    length: 3,
    convRule: rule124
  }, {
    start: 70371,
    length: 8,
    convRule: rule92
  }, {
    start: 70384,
    length: 10,
    convRule: rule8
  }, {
    start: 70400,
    length: 2,
    convRule: rule92
  }, {
    start: 70402,
    length: 2,
    convRule: rule124
  }, {
    start: 70405,
    length: 8,
    convRule: rule14
  }, {
    start: 70415,
    length: 2,
    convRule: rule14
  }, {
    start: 70419,
    length: 22,
    convRule: rule14
  }, {
    start: 70442,
    length: 7,
    convRule: rule14
  }, {
    start: 70450,
    length: 2,
    convRule: rule14
  }, {
    start: 70453,
    length: 5,
    convRule: rule14
  }, {
    start: 70459,
    length: 2,
    convRule: rule92
  }, {
    start: 70461,
    length: 1,
    convRule: rule14
  }, {
    start: 70462,
    length: 2,
    convRule: rule124
  }, {
    start: 70464,
    length: 1,
    convRule: rule92
  }, {
    start: 70465,
    length: 4,
    convRule: rule124
  }, {
    start: 70471,
    length: 2,
    convRule: rule124
  }, {
    start: 70475,
    length: 3,
    convRule: rule124
  }, {
    start: 70480,
    length: 1,
    convRule: rule14
  }, {
    start: 70487,
    length: 1,
    convRule: rule124
  }, {
    start: 70493,
    length: 5,
    convRule: rule14
  }, {
    start: 70498,
    length: 2,
    convRule: rule124
  }, {
    start: 70502,
    length: 7,
    convRule: rule92
  }, {
    start: 70512,
    length: 5,
    convRule: rule92
  }, {
    start: 70656,
    length: 53,
    convRule: rule14
  }, {
    start: 70709,
    length: 3,
    convRule: rule124
  }, {
    start: 70712,
    length: 8,
    convRule: rule92
  }, {
    start: 70720,
    length: 2,
    convRule: rule124
  }, {
    start: 70722,
    length: 3,
    convRule: rule92
  }, {
    start: 70725,
    length: 1,
    convRule: rule124
  }, {
    start: 70726,
    length: 1,
    convRule: rule92
  }, {
    start: 70727,
    length: 4,
    convRule: rule14
  }, {
    start: 70731,
    length: 5,
    convRule: rule2
  }, {
    start: 70736,
    length: 10,
    convRule: rule8
  }, {
    start: 70746,
    length: 2,
    convRule: rule2
  }, {
    start: 70749,
    length: 1,
    convRule: rule2
  }, {
    start: 70750,
    length: 1,
    convRule: rule92
  }, {
    start: 70751,
    length: 3,
    convRule: rule14
  }, {
    start: 70784,
    length: 48,
    convRule: rule14
  }, {
    start: 70832,
    length: 3,
    convRule: rule124
  }, {
    start: 70835,
    length: 6,
    convRule: rule92
  }, {
    start: 70841,
    length: 1,
    convRule: rule124
  }, {
    start: 70842,
    length: 1,
    convRule: rule92
  }, {
    start: 70843,
    length: 4,
    convRule: rule124
  }, {
    start: 70847,
    length: 2,
    convRule: rule92
  }, {
    start: 70849,
    length: 1,
    convRule: rule124
  }, {
    start: 70850,
    length: 2,
    convRule: rule92
  }, {
    start: 70852,
    length: 2,
    convRule: rule14
  }, {
    start: 70854,
    length: 1,
    convRule: rule2
  }, {
    start: 70855,
    length: 1,
    convRule: rule14
  }, {
    start: 70864,
    length: 10,
    convRule: rule8
  }, {
    start: 71040,
    length: 47,
    convRule: rule14
  }, {
    start: 71087,
    length: 3,
    convRule: rule124
  }, {
    start: 71090,
    length: 4,
    convRule: rule92
  }, {
    start: 71096,
    length: 4,
    convRule: rule124
  }, {
    start: 71100,
    length: 2,
    convRule: rule92
  }, {
    start: 71102,
    length: 1,
    convRule: rule124
  }, {
    start: 71103,
    length: 2,
    convRule: rule92
  }, {
    start: 71105,
    length: 23,
    convRule: rule2
  }, {
    start: 71128,
    length: 4,
    convRule: rule14
  }, {
    start: 71132,
    length: 2,
    convRule: rule92
  }, {
    start: 71168,
    length: 48,
    convRule: rule14
  }, {
    start: 71216,
    length: 3,
    convRule: rule124
  }, {
    start: 71219,
    length: 8,
    convRule: rule92
  }, {
    start: 71227,
    length: 2,
    convRule: rule124
  }, {
    start: 71229,
    length: 1,
    convRule: rule92
  }, {
    start: 71230,
    length: 1,
    convRule: rule124
  }, {
    start: 71231,
    length: 2,
    convRule: rule92
  }, {
    start: 71233,
    length: 3,
    convRule: rule2
  }, {
    start: 71236,
    length: 1,
    convRule: rule14
  }, {
    start: 71248,
    length: 10,
    convRule: rule8
  }, {
    start: 71264,
    length: 13,
    convRule: rule2
  }, {
    start: 71296,
    length: 43,
    convRule: rule14
  }, {
    start: 71339,
    length: 1,
    convRule: rule92
  }, {
    start: 71340,
    length: 1,
    convRule: rule124
  }, {
    start: 71341,
    length: 1,
    convRule: rule92
  }, {
    start: 71342,
    length: 2,
    convRule: rule124
  }, {
    start: 71344,
    length: 6,
    convRule: rule92
  }, {
    start: 71350,
    length: 1,
    convRule: rule124
  }, {
    start: 71351,
    length: 1,
    convRule: rule92
  }, {
    start: 71352,
    length: 1,
    convRule: rule14
  }, {
    start: 71360,
    length: 10,
    convRule: rule8
  }, {
    start: 71424,
    length: 27,
    convRule: rule14
  }, {
    start: 71453,
    length: 3,
    convRule: rule92
  }, {
    start: 71456,
    length: 2,
    convRule: rule124
  }, {
    start: 71458,
    length: 4,
    convRule: rule92
  }, {
    start: 71462,
    length: 1,
    convRule: rule124
  }, {
    start: 71463,
    length: 5,
    convRule: rule92
  }, {
    start: 71472,
    length: 10,
    convRule: rule8
  }, {
    start: 71482,
    length: 2,
    convRule: rule17
  }, {
    start: 71484,
    length: 3,
    convRule: rule2
  }, {
    start: 71487,
    length: 1,
    convRule: rule13
  }, {
    start: 71680,
    length: 44,
    convRule: rule14
  }, {
    start: 71724,
    length: 3,
    convRule: rule124
  }, {
    start: 71727,
    length: 9,
    convRule: rule92
  }, {
    start: 71736,
    length: 1,
    convRule: rule124
  }, {
    start: 71737,
    length: 2,
    convRule: rule92
  }, {
    start: 71739,
    length: 1,
    convRule: rule2
  }, {
    start: 71840,
    length: 32,
    convRule: rule9
  }, {
    start: 71872,
    length: 32,
    convRule: rule12
  }, {
    start: 71904,
    length: 10,
    convRule: rule8
  }, {
    start: 71914,
    length: 9,
    convRule: rule17
  }, {
    start: 71935,
    length: 8,
    convRule: rule14
  }, {
    start: 71945,
    length: 1,
    convRule: rule14
  }, {
    start: 71948,
    length: 8,
    convRule: rule14
  }, {
    start: 71957,
    length: 2,
    convRule: rule14
  }, {
    start: 71960,
    length: 24,
    convRule: rule14
  }, {
    start: 71984,
    length: 6,
    convRule: rule124
  }, {
    start: 71991,
    length: 2,
    convRule: rule124
  }, {
    start: 71995,
    length: 2,
    convRule: rule92
  }, {
    start: 71997,
    length: 1,
    convRule: rule124
  }, {
    start: 71998,
    length: 1,
    convRule: rule92
  }, {
    start: 71999,
    length: 1,
    convRule: rule14
  }, {
    start: 72e3,
    length: 1,
    convRule: rule124
  }, {
    start: 72001,
    length: 1,
    convRule: rule14
  }, {
    start: 72002,
    length: 1,
    convRule: rule124
  }, {
    start: 72003,
    length: 1,
    convRule: rule92
  }, {
    start: 72004,
    length: 3,
    convRule: rule2
  }, {
    start: 72016,
    length: 10,
    convRule: rule8
  }, {
    start: 72096,
    length: 8,
    convRule: rule14
  }, {
    start: 72106,
    length: 39,
    convRule: rule14
  }, {
    start: 72145,
    length: 3,
    convRule: rule124
  }, {
    start: 72148,
    length: 4,
    convRule: rule92
  }, {
    start: 72154,
    length: 2,
    convRule: rule92
  }, {
    start: 72156,
    length: 4,
    convRule: rule124
  }, {
    start: 72160,
    length: 1,
    convRule: rule92
  }, {
    start: 72161,
    length: 1,
    convRule: rule14
  }, {
    start: 72162,
    length: 1,
    convRule: rule2
  }, {
    start: 72163,
    length: 1,
    convRule: rule14
  }, {
    start: 72164,
    length: 1,
    convRule: rule124
  }, {
    start: 72192,
    length: 1,
    convRule: rule14
  }, {
    start: 72193,
    length: 10,
    convRule: rule92
  }, {
    start: 72203,
    length: 40,
    convRule: rule14
  }, {
    start: 72243,
    length: 6,
    convRule: rule92
  }, {
    start: 72249,
    length: 1,
    convRule: rule124
  }, {
    start: 72250,
    length: 1,
    convRule: rule14
  }, {
    start: 72251,
    length: 4,
    convRule: rule92
  }, {
    start: 72255,
    length: 8,
    convRule: rule2
  }, {
    start: 72263,
    length: 1,
    convRule: rule92
  }, {
    start: 72272,
    length: 1,
    convRule: rule14
  }, {
    start: 72273,
    length: 6,
    convRule: rule92
  }, {
    start: 72279,
    length: 2,
    convRule: rule124
  }, {
    start: 72281,
    length: 3,
    convRule: rule92
  }, {
    start: 72284,
    length: 46,
    convRule: rule14
  }, {
    start: 72330,
    length: 13,
    convRule: rule92
  }, {
    start: 72343,
    length: 1,
    convRule: rule124
  }, {
    start: 72344,
    length: 2,
    convRule: rule92
  }, {
    start: 72346,
    length: 3,
    convRule: rule2
  }, {
    start: 72349,
    length: 1,
    convRule: rule14
  }, {
    start: 72350,
    length: 5,
    convRule: rule2
  }, {
    start: 72384,
    length: 57,
    convRule: rule14
  }, {
    start: 72704,
    length: 9,
    convRule: rule14
  }, {
    start: 72714,
    length: 37,
    convRule: rule14
  }, {
    start: 72751,
    length: 1,
    convRule: rule124
  }, {
    start: 72752,
    length: 7,
    convRule: rule92
  }, {
    start: 72760,
    length: 6,
    convRule: rule92
  }, {
    start: 72766,
    length: 1,
    convRule: rule124
  }, {
    start: 72767,
    length: 1,
    convRule: rule92
  }, {
    start: 72768,
    length: 1,
    convRule: rule14
  }, {
    start: 72769,
    length: 5,
    convRule: rule2
  }, {
    start: 72784,
    length: 10,
    convRule: rule8
  }, {
    start: 72794,
    length: 19,
    convRule: rule17
  }, {
    start: 72816,
    length: 2,
    convRule: rule2
  }, {
    start: 72818,
    length: 30,
    convRule: rule14
  }, {
    start: 72850,
    length: 22,
    convRule: rule92
  }, {
    start: 72873,
    length: 1,
    convRule: rule124
  }, {
    start: 72874,
    length: 7,
    convRule: rule92
  }, {
    start: 72881,
    length: 1,
    convRule: rule124
  }, {
    start: 72882,
    length: 2,
    convRule: rule92
  }, {
    start: 72884,
    length: 1,
    convRule: rule124
  }, {
    start: 72885,
    length: 2,
    convRule: rule92
  }, {
    start: 72960,
    length: 7,
    convRule: rule14
  }, {
    start: 72968,
    length: 2,
    convRule: rule14
  }, {
    start: 72971,
    length: 38,
    convRule: rule14
  }, {
    start: 73009,
    length: 6,
    convRule: rule92
  }, {
    start: 73018,
    length: 1,
    convRule: rule92
  }, {
    start: 73020,
    length: 2,
    convRule: rule92
  }, {
    start: 73023,
    length: 7,
    convRule: rule92
  }, {
    start: 73030,
    length: 1,
    convRule: rule14
  }, {
    start: 73031,
    length: 1,
    convRule: rule92
  }, {
    start: 73040,
    length: 10,
    convRule: rule8
  }, {
    start: 73056,
    length: 6,
    convRule: rule14
  }, {
    start: 73063,
    length: 2,
    convRule: rule14
  }, {
    start: 73066,
    length: 32,
    convRule: rule14
  }, {
    start: 73098,
    length: 5,
    convRule: rule124
  }, {
    start: 73104,
    length: 2,
    convRule: rule92
  }, {
    start: 73107,
    length: 2,
    convRule: rule124
  }, {
    start: 73109,
    length: 1,
    convRule: rule92
  }, {
    start: 73110,
    length: 1,
    convRule: rule124
  }, {
    start: 73111,
    length: 1,
    convRule: rule92
  }, {
    start: 73112,
    length: 1,
    convRule: rule14
  }, {
    start: 73120,
    length: 10,
    convRule: rule8
  }, {
    start: 73440,
    length: 19,
    convRule: rule14
  }, {
    start: 73459,
    length: 2,
    convRule: rule92
  }, {
    start: 73461,
    length: 2,
    convRule: rule124
  }, {
    start: 73463,
    length: 2,
    convRule: rule2
  }, {
    start: 73648,
    length: 1,
    convRule: rule14
  }, {
    start: 73664,
    length: 21,
    convRule: rule17
  }, {
    start: 73685,
    length: 8,
    convRule: rule13
  }, {
    start: 73693,
    length: 4,
    convRule: rule3
  }, {
    start: 73697,
    length: 17,
    convRule: rule13
  }, {
    start: 73727,
    length: 1,
    convRule: rule2
  }, {
    start: 73728,
    length: 922,
    convRule: rule14
  }, {
    start: 74752,
    length: 111,
    convRule: rule128
  }, {
    start: 74864,
    length: 5,
    convRule: rule2
  }, {
    start: 74880,
    length: 196,
    convRule: rule14
  }, {
    start: 77824,
    length: 1071,
    convRule: rule14
  }, {
    start: 78896,
    length: 9,
    convRule: rule16
  }, {
    start: 82944,
    length: 583,
    convRule: rule14
  }, {
    start: 92160,
    length: 569,
    convRule: rule14
  }, {
    start: 92736,
    length: 31,
    convRule: rule14
  }, {
    start: 92768,
    length: 10,
    convRule: rule8
  }, {
    start: 92782,
    length: 2,
    convRule: rule2
  }, {
    start: 92880,
    length: 30,
    convRule: rule14
  }, {
    start: 92912,
    length: 5,
    convRule: rule92
  }, {
    start: 92917,
    length: 1,
    convRule: rule2
  }, {
    start: 92928,
    length: 48,
    convRule: rule14
  }, {
    start: 92976,
    length: 7,
    convRule: rule92
  }, {
    start: 92983,
    length: 5,
    convRule: rule2
  }, {
    start: 92988,
    length: 4,
    convRule: rule13
  }, {
    start: 92992,
    length: 4,
    convRule: rule91
  }, {
    start: 92996,
    length: 1,
    convRule: rule2
  }, {
    start: 92997,
    length: 1,
    convRule: rule13
  }, {
    start: 93008,
    length: 10,
    convRule: rule8
  }, {
    start: 93019,
    length: 7,
    convRule: rule17
  }, {
    start: 93027,
    length: 21,
    convRule: rule14
  }, {
    start: 93053,
    length: 19,
    convRule: rule14
  }, {
    start: 93760,
    length: 32,
    convRule: rule9
  }, {
    start: 93792,
    length: 32,
    convRule: rule12
  }, {
    start: 93824,
    length: 23,
    convRule: rule17
  }, {
    start: 93847,
    length: 4,
    convRule: rule2
  }, {
    start: 93952,
    length: 75,
    convRule: rule14
  }, {
    start: 94031,
    length: 1,
    convRule: rule92
  }, {
    start: 94032,
    length: 1,
    convRule: rule14
  }, {
    start: 94033,
    length: 55,
    convRule: rule124
  }, {
    start: 94095,
    length: 4,
    convRule: rule92
  }, {
    start: 94099,
    length: 13,
    convRule: rule91
  }, {
    start: 94176,
    length: 2,
    convRule: rule91
  }, {
    start: 94178,
    length: 1,
    convRule: rule2
  }, {
    start: 94179,
    length: 1,
    convRule: rule91
  }, {
    start: 94180,
    length: 1,
    convRule: rule92
  }, {
    start: 94192,
    length: 2,
    convRule: rule124
  }, {
    start: 94208,
    length: 6136,
    convRule: rule14
  }, {
    start: 100352,
    length: 1238,
    convRule: rule14
  }, {
    start: 101632,
    length: 9,
    convRule: rule14
  }, {
    start: 110592,
    length: 287,
    convRule: rule14
  }, {
    start: 110928,
    length: 3,
    convRule: rule14
  }, {
    start: 110948,
    length: 4,
    convRule: rule14
  }, {
    start: 110960,
    length: 396,
    convRule: rule14
  }, {
    start: 113664,
    length: 107,
    convRule: rule14
  }, {
    start: 113776,
    length: 13,
    convRule: rule14
  }, {
    start: 113792,
    length: 9,
    convRule: rule14
  }, {
    start: 113808,
    length: 10,
    convRule: rule14
  }, {
    start: 113820,
    length: 1,
    convRule: rule13
  }, {
    start: 113821,
    length: 2,
    convRule: rule92
  }, {
    start: 113823,
    length: 1,
    convRule: rule2
  }, {
    start: 113824,
    length: 4,
    convRule: rule16
  }, {
    start: 118784,
    length: 246,
    convRule: rule13
  }, {
    start: 119040,
    length: 39,
    convRule: rule13
  }, {
    start: 119081,
    length: 60,
    convRule: rule13
  }, {
    start: 119141,
    length: 2,
    convRule: rule124
  }, {
    start: 119143,
    length: 3,
    convRule: rule92
  }, {
    start: 119146,
    length: 3,
    convRule: rule13
  }, {
    start: 119149,
    length: 6,
    convRule: rule124
  }, {
    start: 119155,
    length: 8,
    convRule: rule16
  }, {
    start: 119163,
    length: 8,
    convRule: rule92
  }, {
    start: 119171,
    length: 2,
    convRule: rule13
  }, {
    start: 119173,
    length: 7,
    convRule: rule92
  }, {
    start: 119180,
    length: 30,
    convRule: rule13
  }, {
    start: 119210,
    length: 4,
    convRule: rule92
  }, {
    start: 119214,
    length: 59,
    convRule: rule13
  }, {
    start: 119296,
    length: 66,
    convRule: rule13
  }, {
    start: 119362,
    length: 3,
    convRule: rule92
  }, {
    start: 119365,
    length: 1,
    convRule: rule13
  }, {
    start: 119520,
    length: 20,
    convRule: rule17
  }, {
    start: 119552,
    length: 87,
    convRule: rule13
  }, {
    start: 119648,
    length: 25,
    convRule: rule17
  }, {
    start: 119808,
    length: 26,
    convRule: rule107
  }, {
    start: 119834,
    length: 26,
    convRule: rule20
  }, {
    start: 119860,
    length: 26,
    convRule: rule107
  }, {
    start: 119886,
    length: 7,
    convRule: rule20
  }, {
    start: 119894,
    length: 18,
    convRule: rule20
  }, {
    start: 119912,
    length: 26,
    convRule: rule107
  }, {
    start: 119938,
    length: 26,
    convRule: rule20
  }, {
    start: 119964,
    length: 1,
    convRule: rule107
  }, {
    start: 119966,
    length: 2,
    convRule: rule107
  }, {
    start: 119970,
    length: 1,
    convRule: rule107
  }, {
    start: 119973,
    length: 2,
    convRule: rule107
  }, {
    start: 119977,
    length: 4,
    convRule: rule107
  }, {
    start: 119982,
    length: 8,
    convRule: rule107
  }, {
    start: 119990,
    length: 4,
    convRule: rule20
  }, {
    start: 119995,
    length: 1,
    convRule: rule20
  }, {
    start: 119997,
    length: 7,
    convRule: rule20
  }, {
    start: 120005,
    length: 11,
    convRule: rule20
  }, {
    start: 120016,
    length: 26,
    convRule: rule107
  }, {
    start: 120042,
    length: 26,
    convRule: rule20
  }, {
    start: 120068,
    length: 2,
    convRule: rule107
  }, {
    start: 120071,
    length: 4,
    convRule: rule107
  }, {
    start: 120077,
    length: 8,
    convRule: rule107
  }, {
    start: 120086,
    length: 7,
    convRule: rule107
  }, {
    start: 120094,
    length: 26,
    convRule: rule20
  }, {
    start: 120120,
    length: 2,
    convRule: rule107
  }, {
    start: 120123,
    length: 4,
    convRule: rule107
  }, {
    start: 120128,
    length: 5,
    convRule: rule107
  }, {
    start: 120134,
    length: 1,
    convRule: rule107
  }, {
    start: 120138,
    length: 7,
    convRule: rule107
  }, {
    start: 120146,
    length: 26,
    convRule: rule20
  }, {
    start: 120172,
    length: 26,
    convRule: rule107
  }, {
    start: 120198,
    length: 26,
    convRule: rule20
  }, {
    start: 120224,
    length: 26,
    convRule: rule107
  }, {
    start: 120250,
    length: 26,
    convRule: rule20
  }, {
    start: 120276,
    length: 26,
    convRule: rule107
  }, {
    start: 120302,
    length: 26,
    convRule: rule20
  }, {
    start: 120328,
    length: 26,
    convRule: rule107
  }, {
    start: 120354,
    length: 26,
    convRule: rule20
  }, {
    start: 120380,
    length: 26,
    convRule: rule107
  }, {
    start: 120406,
    length: 26,
    convRule: rule20
  }, {
    start: 120432,
    length: 26,
    convRule: rule107
  }, {
    start: 120458,
    length: 28,
    convRule: rule20
  }, {
    start: 120488,
    length: 25,
    convRule: rule107
  }, {
    start: 120513,
    length: 1,
    convRule: rule6
  }, {
    start: 120514,
    length: 25,
    convRule: rule20
  }, {
    start: 120539,
    length: 1,
    convRule: rule6
  }, {
    start: 120540,
    length: 6,
    convRule: rule20
  }, {
    start: 120546,
    length: 25,
    convRule: rule107
  }, {
    start: 120571,
    length: 1,
    convRule: rule6
  }, {
    start: 120572,
    length: 25,
    convRule: rule20
  }, {
    start: 120597,
    length: 1,
    convRule: rule6
  }, {
    start: 120598,
    length: 6,
    convRule: rule20
  }, {
    start: 120604,
    length: 25,
    convRule: rule107
  }, {
    start: 120629,
    length: 1,
    convRule: rule6
  }, {
    start: 120630,
    length: 25,
    convRule: rule20
  }, {
    start: 120655,
    length: 1,
    convRule: rule6
  }, {
    start: 120656,
    length: 6,
    convRule: rule20
  }, {
    start: 120662,
    length: 25,
    convRule: rule107
  }, {
    start: 120687,
    length: 1,
    convRule: rule6
  }, {
    start: 120688,
    length: 25,
    convRule: rule20
  }, {
    start: 120713,
    length: 1,
    convRule: rule6
  }, {
    start: 120714,
    length: 6,
    convRule: rule20
  }, {
    start: 120720,
    length: 25,
    convRule: rule107
  }, {
    start: 120745,
    length: 1,
    convRule: rule6
  }, {
    start: 120746,
    length: 25,
    convRule: rule20
  }, {
    start: 120771,
    length: 1,
    convRule: rule6
  }, {
    start: 120772,
    length: 6,
    convRule: rule20
  }, {
    start: 120778,
    length: 1,
    convRule: rule107
  }, {
    start: 120779,
    length: 1,
    convRule: rule20
  }, {
    start: 120782,
    length: 50,
    convRule: rule8
  }, {
    start: 120832,
    length: 512,
    convRule: rule13
  }, {
    start: 121344,
    length: 55,
    convRule: rule92
  }, {
    start: 121399,
    length: 4,
    convRule: rule13
  }, {
    start: 121403,
    length: 50,
    convRule: rule92
  }, {
    start: 121453,
    length: 8,
    convRule: rule13
  }, {
    start: 121461,
    length: 1,
    convRule: rule92
  }, {
    start: 121462,
    length: 14,
    convRule: rule13
  }, {
    start: 121476,
    length: 1,
    convRule: rule92
  }, {
    start: 121477,
    length: 2,
    convRule: rule13
  }, {
    start: 121479,
    length: 5,
    convRule: rule2
  }, {
    start: 121499,
    length: 5,
    convRule: rule92
  }, {
    start: 121505,
    length: 15,
    convRule: rule92
  }, {
    start: 122880,
    length: 7,
    convRule: rule92
  }, {
    start: 122888,
    length: 17,
    convRule: rule92
  }, {
    start: 122907,
    length: 7,
    convRule: rule92
  }, {
    start: 122915,
    length: 2,
    convRule: rule92
  }, {
    start: 122918,
    length: 5,
    convRule: rule92
  }, {
    start: 123136,
    length: 45,
    convRule: rule14
  }, {
    start: 123184,
    length: 7,
    convRule: rule92
  }, {
    start: 123191,
    length: 7,
    convRule: rule91
  }, {
    start: 123200,
    length: 10,
    convRule: rule8
  }, {
    start: 123214,
    length: 1,
    convRule: rule14
  }, {
    start: 123215,
    length: 1,
    convRule: rule13
  }, {
    start: 123584,
    length: 44,
    convRule: rule14
  }, {
    start: 123628,
    length: 4,
    convRule: rule92
  }, {
    start: 123632,
    length: 10,
    convRule: rule8
  }, {
    start: 123647,
    length: 1,
    convRule: rule3
  }, {
    start: 124928,
    length: 197,
    convRule: rule14
  }, {
    start: 125127,
    length: 9,
    convRule: rule17
  }, {
    start: 125136,
    length: 7,
    convRule: rule92
  }, {
    start: 125184,
    length: 34,
    convRule: rule203
  }, {
    start: 125218,
    length: 34,
    convRule: rule204
  }, {
    start: 125252,
    length: 7,
    convRule: rule92
  }, {
    start: 125259,
    length: 1,
    convRule: rule91
  }, {
    start: 125264,
    length: 10,
    convRule: rule8
  }, {
    start: 125278,
    length: 2,
    convRule: rule2
  }, {
    start: 126065,
    length: 59,
    convRule: rule17
  }, {
    start: 126124,
    length: 1,
    convRule: rule13
  }, {
    start: 126125,
    length: 3,
    convRule: rule17
  }, {
    start: 126128,
    length: 1,
    convRule: rule3
  }, {
    start: 126129,
    length: 4,
    convRule: rule17
  }, {
    start: 126209,
    length: 45,
    convRule: rule17
  }, {
    start: 126254,
    length: 1,
    convRule: rule13
  }, {
    start: 126255,
    length: 15,
    convRule: rule17
  }, {
    start: 126464,
    length: 4,
    convRule: rule14
  }, {
    start: 126469,
    length: 27,
    convRule: rule14
  }, {
    start: 126497,
    length: 2,
    convRule: rule14
  }, {
    start: 126500,
    length: 1,
    convRule: rule14
  }, {
    start: 126503,
    length: 1,
    convRule: rule14
  }, {
    start: 126505,
    length: 10,
    convRule: rule14
  }, {
    start: 126516,
    length: 4,
    convRule: rule14
  }, {
    start: 126521,
    length: 1,
    convRule: rule14
  }, {
    start: 126523,
    length: 1,
    convRule: rule14
  }, {
    start: 126530,
    length: 1,
    convRule: rule14
  }, {
    start: 126535,
    length: 1,
    convRule: rule14
  }, {
    start: 126537,
    length: 1,
    convRule: rule14
  }, {
    start: 126539,
    length: 1,
    convRule: rule14
  }, {
    start: 126541,
    length: 3,
    convRule: rule14
  }, {
    start: 126545,
    length: 2,
    convRule: rule14
  }, {
    start: 126548,
    length: 1,
    convRule: rule14
  }, {
    start: 126551,
    length: 1,
    convRule: rule14
  }, {
    start: 126553,
    length: 1,
    convRule: rule14
  }, {
    start: 126555,
    length: 1,
    convRule: rule14
  }, {
    start: 126557,
    length: 1,
    convRule: rule14
  }, {
    start: 126559,
    length: 1,
    convRule: rule14
  }, {
    start: 126561,
    length: 2,
    convRule: rule14
  }, {
    start: 126564,
    length: 1,
    convRule: rule14
  }, {
    start: 126567,
    length: 4,
    convRule: rule14
  }, {
    start: 126572,
    length: 7,
    convRule: rule14
  }, {
    start: 126580,
    length: 4,
    convRule: rule14
  }, {
    start: 126585,
    length: 4,
    convRule: rule14
  }, {
    start: 126590,
    length: 1,
    convRule: rule14
  }, {
    start: 126592,
    length: 10,
    convRule: rule14
  }, {
    start: 126603,
    length: 17,
    convRule: rule14
  }, {
    start: 126625,
    length: 3,
    convRule: rule14
  }, {
    start: 126629,
    length: 5,
    convRule: rule14
  }, {
    start: 126635,
    length: 17,
    convRule: rule14
  }, {
    start: 126704,
    length: 2,
    convRule: rule6
  }, {
    start: 126976,
    length: 44,
    convRule: rule13
  }, {
    start: 127024,
    length: 100,
    convRule: rule13
  }, {
    start: 127136,
    length: 15,
    convRule: rule13
  }, {
    start: 127153,
    length: 15,
    convRule: rule13
  }, {
    start: 127169,
    length: 15,
    convRule: rule13
  }, {
    start: 127185,
    length: 37,
    convRule: rule13
  }, {
    start: 127232,
    length: 13,
    convRule: rule17
  }, {
    start: 127245,
    length: 161,
    convRule: rule13
  }, {
    start: 127462,
    length: 29,
    convRule: rule13
  }, {
    start: 127504,
    length: 44,
    convRule: rule13
  }, {
    start: 127552,
    length: 9,
    convRule: rule13
  }, {
    start: 127568,
    length: 2,
    convRule: rule13
  }, {
    start: 127584,
    length: 6,
    convRule: rule13
  }, {
    start: 127744,
    length: 251,
    convRule: rule13
  }, {
    start: 127995,
    length: 5,
    convRule: rule10
  }, {
    start: 128e3,
    length: 728,
    convRule: rule13
  }, {
    start: 128736,
    length: 13,
    convRule: rule13
  }, {
    start: 128752,
    length: 13,
    convRule: rule13
  }, {
    start: 128768,
    length: 116,
    convRule: rule13
  }, {
    start: 128896,
    length: 89,
    convRule: rule13
  }, {
    start: 128992,
    length: 12,
    convRule: rule13
  }, {
    start: 129024,
    length: 12,
    convRule: rule13
  }, {
    start: 129040,
    length: 56,
    convRule: rule13
  }, {
    start: 129104,
    length: 10,
    convRule: rule13
  }, {
    start: 129120,
    length: 40,
    convRule: rule13
  }, {
    start: 129168,
    length: 30,
    convRule: rule13
  }, {
    start: 129200,
    length: 2,
    convRule: rule13
  }, {
    start: 129280,
    length: 121,
    convRule: rule13
  }, {
    start: 129402,
    length: 82,
    convRule: rule13
  }, {
    start: 129485,
    length: 135,
    convRule: rule13
  }, {
    start: 129632,
    length: 14,
    convRule: rule13
  }, {
    start: 129648,
    length: 5,
    convRule: rule13
  }, {
    start: 129656,
    length: 3,
    convRule: rule13
  }, {
    start: 129664,
    length: 7,
    convRule: rule13
  }, {
    start: 129680,
    length: 25,
    convRule: rule13
  }, {
    start: 129712,
    length: 7,
    convRule: rule13
  }, {
    start: 129728,
    length: 3,
    convRule: rule13
  }, {
    start: 129744,
    length: 7,
    convRule: rule13
  }, {
    start: 129792,
    length: 147,
    convRule: rule13
  }, {
    start: 129940,
    length: 55,
    convRule: rule13
  }, {
    start: 130032,
    length: 10,
    convRule: rule8
  }, {
    start: 131072,
    length: 42718,
    convRule: rule14
  }, {
    start: 173824,
    length: 4149,
    convRule: rule14
  }, {
    start: 177984,
    length: 222,
    convRule: rule14
  }, {
    start: 178208,
    length: 5762,
    convRule: rule14
  }, {
    start: 183984,
    length: 7473,
    convRule: rule14
  }, {
    start: 194560,
    length: 542,
    convRule: rule14
  }, {
    start: 196608,
    length: 4939,
    convRule: rule14
  }, {
    start: 917505,
    length: 1,
    convRule: rule16
  }, {
    start: 917536,
    length: 96,
    convRule: rule16
  }, {
    start: 917760,
    length: 240,
    convRule: rule92
  }, {
    start: 983040,
    length: 65534,
    convRule: rule200
  }, {
    start: 1048576,
    length: 65534,
    convRule: rule200
  }];
  var checkAttr = function(categories) {
    return function($$char2) {
      var numOfBlocks = function() {
        var $43 = $$char2 < 256;
        if ($43) {
          return numLat1Blocks;
        }
        ;
        return numBlocks;
      }();
      var maybeConversionRule = getRule(allchars)($$char2)(numOfBlocks);
      if (maybeConversionRule instanceof Nothing) {
        return false;
      }
      ;
      if (maybeConversionRule instanceof Just) {
        return isJust(elemIndex2(maybeConversionRule.value0.category)(categories));
      }
      ;
      throw new Error("Failed pattern match at Data.CodePoint.Unicode.Internal (line 5645, column 5 - line 5647, column 86): " + [maybeConversionRule.constructor.name]);
    };
  };
  var uIswalnum = /* @__PURE__ */ checkAttr([gencatLT, gencatLU, gencatLL, gencatLM, gencatLO, gencatMC, gencatME, gencatMN, gencatNO, gencatND, gencatNL]);
  var uIswprint = /* @__PURE__ */ checkAttr([gencatMC, gencatNO, gencatSK, gencatME, gencatND, gencatPO, gencatLT, gencatPC, gencatSM, gencatZS, gencatLU, gencatPD, gencatSO, gencatPE, gencatPF, gencatPS, gencatSC, gencatLL, gencatLM, gencatPI, gencatNL, gencatMN, gencatLO]);

  // output/Data.CodePoint.Unicode/index.js
  var fromEnum4 = /* @__PURE__ */ fromEnum(boundedEnumCodePoint);
  var isPrint = function($67) {
    return uIswprint(fromEnum4($67));
  };
  var isDecDigit = function(c) {
    var diff3 = fromEnum4(c) - toCharCode2("0") | 0;
    return diff3 <= 9 && diff3 >= 0;
  };
  var isHexDigit = function(c) {
    return isDecDigit(c) || (function() {
      var diff3 = fromEnum4(c) - toCharCode2("A") | 0;
      return diff3 <= 5 && diff3 >= 0;
    }() || function() {
      var diff3 = fromEnum4(c) - toCharCode2("a") | 0;
      return diff3 <= 5 && diff3 >= 0;
    }());
  };
  var isAlphaNum = function($70) {
    return uIswalnum(fromEnum4($70));
  };

  // output/Parsing.String.Basic/index.js
  var satisfyCP = function(p4) {
    return satisfy(function($32) {
      return p4(codePointFromChar($32));
    });
  };
  var hexDigit = /* @__PURE__ */ withErrorMessage(/* @__PURE__ */ satisfyCP(isHexDigit))("hex digit");
  var alphaNum = /* @__PURE__ */ withErrorMessage(/* @__PURE__ */ satisfyCP(isAlphaNum))("letter or digit");

  // output/Web.Encoding.TextDecoder/foreign.js
  var newImpl2 = function(label5) {
    return function() {
      return new TextDecoder(label5);
    };
  };
  function _decode(view, options2, decoder) {
    return decoder.decode(view, options2);
  }

  // output/Web.Encoding.TextDecoder/index.js
  var decode = function(arr) {
    return function(decoder) {
      return function() {
        return _decode(arr, {}, decoder);
      };
    };
  };

  // output/Web.Encoding.TextEncoder/foreign.js
  var newImpl3 = function() {
    return new TextEncoder();
  };
  function encode(text6) {
    return function(encoder) {
      return encoder.encode(text6);
    };
  }

  // output/Web.Encoding.UtfLabel/index.js
  var utf8 = "utf8";

  // output/Bp35a1/index.js
  var map20 = /* @__PURE__ */ map(functorMaybe);
  var show3 = /* @__PURE__ */ show(showString);
  var show12 = /* @__PURE__ */ show(uintShowInstance);
  var show23 = /* @__PURE__ */ show(showInt);
  var show32 = /* @__PURE__ */ show(/* @__PURE__ */ showArray(uintShowInstance));
  var $$void9 = /* @__PURE__ */ $$void(functorParserT);
  var applySecond2 = /* @__PURE__ */ applySecond(applyParserT);
  var bind6 = /* @__PURE__ */ bind(bindParserT);
  var liftMaybe2 = /* @__PURE__ */ liftMaybe(monadIdentity);
  var discard5 = /* @__PURE__ */ discard(discardUnit)(bindParserT);
  var pure13 = /* @__PURE__ */ pure(applicativeParserT);
  var many2 = /* @__PURE__ */ many(alternativeParserT)(lazyParserT);
  var map110 = /* @__PURE__ */ map(functorArray);
  var alt6 = /* @__PURE__ */ alt(altParserT);
  var apply2 = /* @__PURE__ */ apply(applyParserT);
  var map23 = /* @__PURE__ */ map(functorParserT);
  var ResLocalEcho = /* @__PURE__ */ function() {
    function ResLocalEcho2(value0) {
      this.value0 = value0;
    }
    ;
    ResLocalEcho2.create = function(value0) {
      return new ResLocalEcho2(value0);
    };
    return ResLocalEcho2;
  }();
  var ResOK = /* @__PURE__ */ function() {
    function ResOK2() {
    }
    ;
    ResOK2.value = new ResOK2();
    return ResOK2;
  }();
  var ResFAIL = /* @__PURE__ */ function() {
    function ResFAIL2(value0) {
      this.value0 = value0;
    }
    ;
    ResFAIL2.create = function(value0) {
      return new ResFAIL2(value0);
    };
    return ResFAIL2;
  }();
  var ResIpV6Address = /* @__PURE__ */ function() {
    function ResIpV6Address2(value0) {
      this.value0 = value0;
    }
    ;
    ResIpV6Address2.create = function(value0) {
      return new ResIpV6Address2(value0);
    };
    return ResIpV6Address2;
  }();
  var ResEVENT = /* @__PURE__ */ function() {
    function ResEVENT2(value0) {
      this.value0 = value0;
    }
    ;
    ResEVENT2.create = function(value0) {
      return new ResEVENT2(value0);
    };
    return ResEVENT2;
  }();
  var ResEPANDESC = /* @__PURE__ */ function() {
    function ResEPANDESC2(value0) {
      this.value0 = value0;
    }
    ;
    ResEPANDESC2.create = function(value0) {
      return new ResEPANDESC2(value0);
    };
    return ResEPANDESC2;
  }();
  var ResERXUDP = /* @__PURE__ */ function() {
    function ResERXUDP2(value0) {
      this.value0 = value0;
    }
    ;
    ResERXUDP2.create = function(value0) {
      return new ResERXUDP2(value0);
    };
    return ResERXUDP2;
  }();
  var ResMessage = /* @__PURE__ */ function() {
    function ResMessage2(value0) {
      this.value0 = value0;
    }
    ;
    ResMessage2.create = function(value0) {
      return new ResMessage2(value0);
    };
    return ResMessage2;
  }();
  var showIpV6Address = {
    show: function(v) {
      return v;
    }
  };
  var show4 = /* @__PURE__ */ show(showIpV6Address);
  var toStringEvent = function(v) {
    var param2 = function(n) {
      return function(p4) {
        if (n === 31) {
          return "\u76F4\u5F8C\u306BEEDSCAN \u30A4\u30D9\u30F3\u30C8\u304C\u767A\u751F\u3057\u307E\u3059";
        }
        ;
        if (n === 32) {
          return "\u76F4\u5F8C\u306BEPANDESC \u30A4\u30D9\u30F3\u30C8\u304C\u767A\u751F\u3057\u307E\u3059";
        }
        ;
        if (n === 33 && p4 === 0) {
          return "UDP \u306E\u9001\u4FE1\u306B\u6210\u529F";
        }
        ;
        if (n === 33 && p4 === 1) {
          return "UDP \u306E\u9001\u4FE1\u306B\u5931\u6557";
        }
        ;
        if (n === 33 && p4 === 2) {
          return "UDP \u3092\u9001\u4FE1\u3059\u308B\u4EE3\u308F\u308A\u306B\u30A2\u30C9\u30EC\u30B9\u8981\u8ACB\uFF08Neighbor Solicitation\uFF09\u3092\u884C\u3063\u305F\u3053\u3068\u3092\u8868\u3057\u307E\u3059\u3002";
        }
        ;
        if (otherwise) {
          return toStringAs(hexadecimal)(p4);
        }
        ;
        throw new Error("Failed pattern match at Bp35a1 (line 134, column 3 - line 140, column 51): " + [n.constructor.name, p4.constructor.name]);
      };
    };
    var eventnum = function(n) {
      if (n === 1) {
        return "EVENT 0x01: NS \u3092\u53D7\u4FE1\u3057\u305F";
      }
      ;
      if (n === 2) {
        return "EVENT 0x02: NA \u3092\u53D7\u4FE1\u3057\u305F";
      }
      ;
      if (n === 5) {
        return "EVENT 0x05: Echo Request \u3092\u53D7\u4FE1\u3057\u305F";
      }
      ;
      if (n === 31) {
        return "EVENT 0x1F: ED \u30B9\u30AD\u30E3\u30F3\u304C\u5B8C\u4E86\u3057\u305F";
      }
      ;
      if (n === 32) {
        return "EVENT 0x20: Beacon \u3092\u53D7\u4FE1\u3057\u305F";
      }
      ;
      if (n === 33) {
        return "EVENT 0x21: UDP \u9001\u4FE1\u51E6\u7406\u304C\u5B8C\u4E86\u3057\u305F";
      }
      ;
      if (n === 34) {
        return "EVENT 0x22: \u30A2\u30AF\u30C6\u30A3\u30D6\u30B9\u30AD\u30E3\u30F3\u304C\u5B8C\u4E86\u3057\u305F";
      }
      ;
      if (n === 36) {
        return "EVENT 0x24: PANA \u306B\u3088\u308B\u63A5\u7D9A\u904E\u7A0B\u3067\u30A8\u30E9\u30FC\u304C\u767A\u751F\u3057\u305F(\u63A5\u7D9A\u304C\u5B8C\u4E86\u3057\u306A\u304B\u3063\u305F)";
      }
      ;
      if (n === 37) {
        return "EVENT 0x25: PANA \u306B\u3088\u308B\u63A5\u7D9A\u304C\u5B8C\u4E86\u3057\u305F";
      }
      ;
      if (n === 38) {
        return "EVENT 0x26: \u63A5\u7D9A\u76F8\u624B\u304B\u3089\u30BB\u30C3\u30B7\u30E7\u30F3\u7D42\u4E86\u8981\u6C42\u3092\u53D7\u4FE1\u3057\u305F";
      }
      ;
      if (n === 39) {
        return "EVENT 0x27: PANA \u30BB\u30C3\u30B7\u30E7\u30F3\u306E\u7D42\u4E86\u306B\u6210\u529F\u3057\u305F";
      }
      ;
      if (n === 40) {
        return "EVENT 0x28: PANA \u30BB\u30C3\u30B7\u30E7\u30F3\u306E\u7D42\u4E86\u8981\u6C42\u306B\u5BFE\u3059\u308B\u5FDC\u7B54\u304C\u306A\u304F\u30BF\u30A4\u30E0\u30A2\u30A6\u30C8\u3057\u305F\uFF08\u30BB\u30C3\u30B7\u30E7\u30F3\u306F\u7D42\u4E86\uFF09";
      }
      ;
      if (n === 41) {
        return "EVENT 0x29: \u30BB\u30C3\u30B7\u30E7\u30F3\u306E\u30E9\u30A4\u30D5\u30BF\u30A4\u30E0\u304C\u7D4C\u904E\u3057\u3066\u671F\u9650\u5207\u308C\u306B\u306A\u3063\u305F";
      }
      ;
      if (n === 50) {
        return "EVENT 0x32: ARIB108 \u306E\u9001\u4FE1\u7DCF\u548C\u6642\u9593\u306E\u5236\u9650\u304C\u767A\u52D5\u3057\u305F\uFF08\u3053\u306E\u30A4\u30D9\u30F3\u30C8\u4EE5\u5F8C\u3001\u3042\u3089\u3086\u308B\u30C7\u30FC\u30BF\u9001\u4FE1\u8981\u6C42\u304C\u5185\u90E8\u3067\u81EA\u52D5\u7684\u306B\u30AD\u30E3\u30F3\u30BB\u30EB\u3055\u308C\u307E\u3059\uFF09";
      }
      ;
      if (n === 51) {
        return "EVENT 0x33: \u9001\u4FE1\u7DCF\u548C\u6642\u9593\u306E\u5236\u9650\u304C\u89E3\u9664\u3055\u308C\u305F";
      }
      ;
      if (otherwise) {
        return "EVENT " + toStringAs(hexadecimal)(n);
      }
      ;
      throw new Error("Failed pattern match at Bp35a1 (line 116, column 3 - line 132, column 63): " + [n.constructor.name]);
    };
    return {
      message: eventnum(v.num),
      sender: show4(v.sender),
      param: map20(param2(v.num))(v.param)
    };
  };
  var showER = {
    show: function(v) {
      if (v === 1) {
        return "ER01: reserved";
      }
      ;
      if (v === 2) {
        return "ER02: reserved";
      }
      ;
      if (v === 3) {
        return "ER03: reserved";
      }
      ;
      if (v === 4) {
        return "ER04: \u6307\u5B9A\u3055\u308C\u305F\u30B3\u30DE\u30F3\u30C9\u304C\u30B5\u30DD\u30FC\u30C8\u3055\u308C\u3066\u3044\u306A\u3044";
      }
      ;
      if (v === 5) {
        return "ER05: \u6307\u5B9A\u3055\u308C\u305F\u30B3\u30DE\u30F3\u30C9\u306E\u5F15\u6570\u306E\u6570\u304C\u6B63\u3057\u304F\u306A\u3044";
      }
      ;
      if (v === 6) {
        return "ER06: \u6307\u5B9A\u3055\u308C\u305F\u30B3\u30DE\u30F3\u30C9\u306E\u5F15\u6570\u5F62\u5F0F\u3084\u5024\u57DF\u304C\u6B63\u3057\u304F\u306A\u3044";
      }
      ;
      if (v === 7) {
        return "ER07: reserved";
      }
      ;
      if (v === 8) {
        return "ER08: reserved";
      }
      ;
      if (v === 9) {
        return "ER09: UART\u5165\u529B\u30A8\u30E9\u30FC\u304C\u767A\u751F\u3057\u305F";
      }
      ;
      if (v === 16) {
        return "ER10: \u6307\u5B9A\u3055\u308C\u305F\u30B3\u30DE\u30F3\u30C9\u306F\u53D7\u3051\u4ED8\u3051\u305F\u304C\u3001\u5B9F\u884C\u7D50\u679C\u304C\u5931\u6557\u3057\u305F";
      }
      ;
      return "ER" + toStringAs(hexadecimal)(v);
    }
  };
  var showAddress64 = {
    show: function(v) {
      return v;
    }
  };
  var show5 = /* @__PURE__ */ show(showAddress64);
  var showEPANDESC = {
    show: function(v) {
      return joinWith(" ")(["EPANDESC", show3(v.channel), show3(v.channelPage), show3(v.panId), show5(v.addr), show12(v.lqi), show3(v.pairId)]);
    }
  };
  var showERXUDP = {
    show: function(v) {
      return joinWith(" ")(["ERXUDP", show4(v.sender), show4(v.dest), show23(v.rport), show23(v.lport), show5(v.senderlla), show23(v.secured), show23(v.datalen), show32(v.payload)]);
    }
  };
  var parseResponce = function(dictMonadEffect) {
    var Monad0 = dictMonadEffect.Monad0();
    var bind110 = bind(Monad0.Bind1());
    var liftEffect10 = liftEffect(dictMonadEffect);
    var pure110 = pure(Monad0.Applicative0());
    return function(lastSendCommand) {
      return function(input3) {
        var go2 = function(string2) {
          var tokenCrLf = $$void9(applySecond2($$char("\r"))($$char("\n")));
          var tokenLocalEcho = bind6(liftMaybe2(function(v) {
            return "this is not echo reply";
          })(lastSendCommand))(function(str) {
            return bind6(string(str))(function() {
              return discard5(tokenCrLf)(function() {
                return bind6(rest)(function(rest2) {
                  return pure13({
                    responce: new ResLocalEcho(str),
                    rest: toCharArray(rest2)
                  });
                });
              });
            });
          });
          var tokenOk = bind6(string("OK"))(function() {
            return discard5(tokenCrLf)(function() {
              return bind6(rest)(function(rest2) {
                return pure13({
                  responce: ResOK.value,
                  rest: toCharArray(rest2)
                });
              });
            });
          });
          var tokenOneLine = bind6(many2(alphaNum))(function(cs) {
            return discard5(tokenCrLf)(function() {
              return bind6(rest)(function(rest2) {
                return pure13({
                  responce: new ResMessage(fromCharArray(cs)),
                  rest: toCharArray(rest2)
                });
              });
            });
          });
          var ipv6addr = function() {
            var twoOctets = bind6(hexDigit)(function(a2) {
              return bind6(hexDigit)(function(b2) {
                return bind6(hexDigit)(function(c) {
                  return bind6(hexDigit)(function(d) {
                    return pure13([a2, b2, c, d]);
                  });
                });
              });
            });
            return bind6(twoOctets)(function(a1) {
              return bind6(applySecond2($$char(":"))(twoOctets))(function(a2) {
                return bind6(applySecond2($$char(":"))(twoOctets))(function(a3) {
                  return bind6(applySecond2($$char(":"))(twoOctets))(function(a4) {
                    return bind6(applySecond2($$char(":"))(twoOctets))(function(a5) {
                      return bind6(applySecond2($$char(":"))(twoOctets))(function(a6) {
                        return bind6(applySecond2($$char(":"))(twoOctets))(function(a7) {
                          return bind6(applySecond2($$char(":"))(twoOctets))(function(a8) {
                            var strings = map110(fromCharArray)([a1, a2, a3, a4, a5, a6, a7, a8]);
                            return pure13(joinWith(":")(strings));
                          });
                        });
                      });
                    });
                  });
                });
              });
            });
          }();
          var hexadecimalDigit = bind6(many2(hexDigit))(function(cs) {
            var v = fromStringAs(hexadecimal)(fromCharArray(cs));
            if (v instanceof Nothing) {
              return fail2("value is not numeric");
            }
            ;
            if (v instanceof Just) {
              return pure13(v.value0);
            }
            ;
            throw new Error("Failed pattern match at Bp35a1 (line 255, column 7 - line 257, column 25): " + [v.constructor.name]);
          });
          var tokenEvent = bind6(string("EVENT"))(function() {
            return bind6($$char(" "))(function() {
              return bind6(hexadecimalDigit)(function(n) {
                return bind6($$char(" "))(function() {
                  return bind6(ipv6addr)(function(addr) {
                    return bind6(optional($$char(" ")))(function() {
                      return bind6(optionMaybe(hexadecimalDigit))(function(param2) {
                        return discard5(tokenCrLf)(function() {
                          return bind6(rest)(function(rest2) {
                            return pure13({
                              responce: new ResEVENT({
                                num: n,
                                sender: addr,
                                param: param2
                              }),
                              rest: toCharArray(rest2)
                            });
                          });
                        });
                      });
                    });
                  });
                });
              });
            });
          });
          var tokenFail = bind6(string("FAIL ER"))(function() {
            return bind6(hexadecimalDigit)(function(n) {
              return discard5(tokenCrLf)(function() {
                return bind6(rest)(function(rest2) {
                  return pure13({
                    responce: new ResFAIL(n),
                    rest: toCharArray(rest2)
                  });
                });
              });
            });
          });
          var address64 = bind6(takeN(16))(function(str) {
            return pure13(str);
          });
          var tokenEpandesc = bind6(string("EPANDESC"))(function() {
            return discard5(tokenCrLf)(function() {
              return bind6(string("  Channel:"))(function() {
                return bind6(many2(hexDigit))(function(channel) {
                  return discard5(tokenCrLf)(function() {
                    return bind6(string("  Channel Page:"))(function() {
                      return bind6(many2(hexDigit))(function(channelPage) {
                        return discard5(tokenCrLf)(function() {
                          return bind6(string("  Pan ID:"))(function() {
                            return bind6(many2(hexDigit))(function(panId) {
                              return discard5(tokenCrLf)(function() {
                                return bind6(string("  Addr:"))(function() {
                                  return bind6(address64)(function(addr) {
                                    return discard5(tokenCrLf)(function() {
                                      return bind6(string("  LQI:"))(function() {
                                        return bind6(hexadecimalDigit)(function(lqi) {
                                          return discard5(tokenCrLf)(function() {
                                            return bind6(string("  PairID:"))(function() {
                                              return bind6(many2(hexDigit))(function(pairId) {
                                                return discard5(tokenCrLf)(function() {
                                                  return bind6(rest)(function(rest2) {
                                                    return pure13({
                                                      responce: new ResEPANDESC({
                                                        channel: fromCharArray(channel),
                                                        channelPage: fromCharArray(channelPage),
                                                        panId: fromCharArray(panId),
                                                        addr,
                                                        lqi: fromInt(lqi),
                                                        pairId: fromCharArray(pairId)
                                                      }),
                                                      rest: toCharArray(rest2)
                                                    });
                                                  });
                                                });
                                              });
                                            });
                                          });
                                        });
                                      });
                                    });
                                  });
                                });
                              });
                            });
                          });
                        });
                      });
                    });
                  });
                });
              });
            });
          });
          var tokenErxudp = function() {
            var octet = bind6(hexDigit)(function(a2) {
              return bind6(hexDigit)(function(b2) {
                var v = map20(fromInt)(fromStringAs(hexadecimal)(fromCharArray([a2, b2])));
                if (v instanceof Nothing) {
                  return fail2("value is not numeric");
                }
                ;
                if (v instanceof Just) {
                  return pure13(v.value0);
                }
                ;
                throw new Error("Failed pattern match at Bp35a1 (line 397, column 9 - line 399, column 27): " + [v.constructor.name]);
              });
            });
            return bind6(string("ERXUDP"))(function() {
              return bind6($$char(" "))(function() {
                return bind6(ipv6addr)(function(sender) {
                  return bind6($$char(" "))(function() {
                    return bind6(ipv6addr)(function(dest) {
                      return bind6($$char(" "))(function() {
                        return bind6(hexadecimalDigit)(function(rport) {
                          return bind6($$char(" "))(function() {
                            return bind6(hexadecimalDigit)(function(lport) {
                              return bind6($$char(" "))(function() {
                                return bind6(address64)(function(senderlla) {
                                  return bind6($$char(" "))(function() {
                                    return bind6(hexadecimalDigit)(function(secured) {
                                      return bind6($$char(" "))(function() {
                                        return bind6(hexadecimalDigit)(function(datalen) {
                                          return bind6($$char(" "))(function() {
                                            return bind6(many2(octet))(function(payload) {
                                              return bind6(rest)(function(rest2) {
                                                return pure13({
                                                  responce: new ResERXUDP({
                                                    sender,
                                                    dest,
                                                    rport,
                                                    lport,
                                                    senderlla,
                                                    secured,
                                                    datalen,
                                                    payload
                                                  }),
                                                  rest: toCharArray(rest2)
                                                });
                                              });
                                            });
                                          });
                                        });
                                      });
                                    });
                                  });
                                });
                              });
                            });
                          });
                        });
                      });
                    });
                  });
                });
              });
            });
          }();
          return runParser(string2)(alt6(tokenLocalEcho)(alt6(tokenOk)(alt6(tokenFail)(alt6(tokenErxudp)(alt6(tokenEvent)(alt6(tokenEpandesc)(alt6(apply2(apply2(map23(function(a2) {
            return function(v) {
              return function(c) {
                return {
                  responce: new ResIpV6Address(a2),
                  rest: toCharArray(c)
                };
              };
            };
          })(ipv6addr))(tokenCrLf))(rest))(alt6(tokenOneLine)(fail2("err"))))))))));
        };
        return bind110(liftEffect10(newImpl2(utf8)))(function(decoder) {
          return bind110(liftEffect10(decode(input3)(decoder)))(function(string2) {
            return pure110(go2(string2));
          });
        });
      };
    };
  };
  var toStringIpV6Address = /* @__PURE__ */ unwrap();
  var makeRSSIfromLQI = function(x) {
    return 0.275 * toNumber2(x) - 104.27;
  };
  var eqIpV6Address = {
    eq: function(x) {
      return function(y) {
        return x === y;
      };
    }
  };

  // output/Data.ArrayBuffer.ArrayBuffer/foreign.js
  function emptyImpl(s) {
    return new ArrayBuffer(s);
  }
  function byteLength(a2) {
    return a2.byteLength;
  }

  // output/Data.ArrayBuffer.ArrayBuffer/index.js
  var empty7 = function(l) {
    return function() {
      return emptyImpl(l);
    };
  };

  // output/Data.ArrayBuffer.DataView/foreign.js
  function whole(b2) {
    return new DataView(b2);
  }
  function buffer(v) {
    return v.buffer;
  }
  function byteOffset(v) {
    return v.byteOffset;
  }
  function byteLength2(v) {
    return v.byteLength;
  }
  function getterImpl(data, v, o) {
    return o + data.bytesPerValue >>> 0 <= v.byteLength ? v[data.functionName].call(v, o, data.littleEndian) : null;
  }
  function setterImpl(data, v, o, n) {
    if (o + data.bytesPerValue >>> 0 <= v.byteLength) {
      v[data.functionName].call(v, o, n, data.littleEndian);
      return true;
    } else {
      return false;
    }
  }

  // output/Data.ArrayBuffer.ValueMapping/index.js
  var bytesPerTypeUint8 = {
    byteWidth: function(v) {
      return 1;
    }
  };
  var bytesPerTypeUint16 = {
    byteWidth: function(v) {
      return 2;
    }
  };
  var bytesPerTypeInt8 = {
    byteWidth: function(v) {
      return 1;
    }
  };
  var byteWidth = function(dict) {
    return dict.byteWidth;
  };

  // output/Data.ArrayBuffer.DataView/index.js
  var map21 = /* @__PURE__ */ map(functorEffect);
  var Uint8IsSymbol = {
    reflectSymbol: function() {
      return "Uint8";
    }
  };
  var LE = /* @__PURE__ */ function() {
    function LE2() {
    }
    ;
    LE2.value = new LE2();
    return LE2;
  }();
  var BE = /* @__PURE__ */ function() {
    function BE2() {
    }
    ;
    BE2.value = new BE2();
    return BE2;
  }();
  var setter = function(dataView) {
    return function(offset) {
      return function(t) {
        return runEffectFn4(setterImpl)(dataView)(offset)(t);
      };
    };
  };
  var getter = function(data$prime) {
    return function(dataView) {
      return function(offset) {
        return map21(toMaybe)(function() {
          return getterImpl({
            functionName: data$prime.functionName,
            littleEndian: data$prime.littleEndian,
            bytesPerValue: data$prime.bytesPerValue
          }, dataView, offset);
        });
      };
    };
  };
  var eqEndian = {
    eq: function(v) {
      return function(v1) {
        if (v instanceof LE && v1 instanceof LE) {
          return true;
        }
        ;
        if (v instanceof BE && v1 instanceof BE) {
          return true;
        }
        ;
        return false;
      };
    }
  };
  var eq3 = /* @__PURE__ */ eq(eqEndian);
  var get2 = function() {
    return function(dictBytesPerType) {
      var byteWidth2 = byteWidth(dictBytesPerType);
      return function() {
        return function(dictIsSymbol) {
          var reflectSymbol2 = reflectSymbol(dictIsSymbol);
          return function(endian) {
            return function(prx) {
              var pnm = "get" + reflectSymbol2($$Proxy.value);
              var le = eq3(endian)(LE.value);
              var bpv = byteWidth2(prx);
              return getter({
                functionName: pnm,
                bytesPerValue: bpv,
                littleEndian: le
              });
            };
          };
        };
      };
    };
  };
  var set = function() {
    return function(dictBytesPerType) {
      var byteWidth2 = byteWidth(dictBytesPerType);
      return function() {
        return function(dictIsSymbol) {
          var reflectSymbol2 = reflectSymbol(dictIsSymbol);
          return function(endian) {
            return function(prx) {
              var pnm = "set" + reflectSymbol2($$Proxy.value);
              var le = eq3(endian)(LE.value);
              var bpv = byteWidth2(prx);
              return setter({
                functionName: pnm,
                bytesPerValue: bpv,
                littleEndian: le
              });
            };
          };
        };
      };
    };
  };
  var set1 = /* @__PURE__ */ set();
  var setLE = function() {
    return function(dictBytesPerType) {
      var set2 = set1(dictBytesPerType)();
      return function() {
        return function(dictIsSymbol) {
          return set2(dictIsSymbol)(LE.value);
        };
      };
    };
  };
  var setLE1 = /* @__PURE__ */ setLE();
  var setUint8 = /* @__PURE__ */ function() {
    return setLE1(bytesPerTypeUint8)()(Uint8IsSymbol)($$Proxy.value);
  }();

  // output/Data.ArrayBuffer.Typed/foreign.js
  function buffer2(v) {
    return v.buffer;
  }
  function byteLength3(v) {
    return v.byteLength;
  }
  function lengthImpl(v) {
    return v.length;
  }
  function newArray(f) {
    return function newArray_(a2, mb, mc) {
      if (mb === null)
        return new f(a2);
      var l = a2.byteLength;
      var eb = f.BYTES_PER_ELEMENT;
      var off = Math.min(l, mb >>> 0);
      if (mc === null)
        return new f(a2, off);
      var len = Math.min((l - off) / eb, mc);
      return new f(a2, off, len);
    };
  }
  var newUint8ClampedArray = newArray(Uint8ClampedArray);
  var newUint32Array = newArray(Uint32Array);
  var newUint16Array = newArray(Uint16Array);
  var newUint8Array = newArray(Uint8Array);
  var newInt32Array = newArray(Int32Array);
  var newInt16Array = newArray(Int16Array);
  var newInt8Array = newArray(Int8Array);
  var newFloat32Array = newArray(Float32Array);
  var newFloat64Array = newArray(Float64Array);
  function findIndexImpl2(a2, f) {
    var r = a2.findIndex(f);
    return r === -1 ? null : r;
  }
  function setImpl(a2, off, b2) {
    a2.set(b2, off);
  }
  function sliceImpl2(a2, s, e) {
    return a2.slice(s, e);
  }

  // output/Data.ArrayBuffer.Typed/index.js
  var pure14 = /* @__PURE__ */ pure(applicativeEffect);
  var applySecond3 = /* @__PURE__ */ applySecond(applyEffect);
  var map111 = /* @__PURE__ */ map(functorEffect);
  var typedArrayUint8 = {
    create: newUint8Array,
    BinaryValue0: function() {
      return void 0;
    }
  };
  var slice3 = function(s) {
    return function(e) {
      return function(a2) {
        return function() {
          return sliceImpl2(a2, s, e);
        };
      };
    };
  };
  var length10 = lengthImpl;
  var setInternal = function(lenfn) {
    return function(a2) {
      return function(mo) {
        return function(b2) {
          var o = fromMaybe(0)(mo);
          var $105 = o >= 0 && lenfn(b2) <= (length10(a2) - o | 0);
          if ($105) {
            return applySecond3(function() {
              return setImpl(a2, o, b2);
            })(pure14(true));
          }
          ;
          return pure14(false);
        };
      };
    };
  };
  var setTyped = /* @__PURE__ */ setInternal(length10);
  var findIndex3 = function(dictTypedArray) {
    return function(f) {
      return function(a2) {
        return map111(toMaybe)(function() {
          return findIndexImpl2(a2, mkFn2(f));
        });
      };
    };
  };
  var create4 = function(dict) {
    return dict.create;
  };
  var part$prime = function(dictTypedArray) {
    var create1 = create4(dictTypedArray);
    return function(a2) {
      return function(x) {
        return function(y) {
          return function() {
            return create1(a2, notNull(x), notNull(y));
          };
        };
      };
    };
  };
  var part = function(dictTypedArray) {
    var part$prime1 = part$prime(dictTypedArray);
    return function(dictBytesPerType) {
      var byteWidth2 = byteWidth(dictBytesPerType);
      return function(a2) {
        return function(x) {
          return function(y) {
            var o = x * byteWidth2($$Proxy.value) | 0;
            return part$prime1(a2)(o)(y);
          };
        };
      };
    };
  };
  var whole2 = function(dictTypedArray) {
    var create1 = create4(dictTypedArray);
    return function(a2) {
      return function() {
        return create1(a2, nullImpl, nullImpl);
      };
    };
  };

  // output/Data.ArrayBuffer.Builder.Internal/index.js
  var unwrap5 = /* @__PURE__ */ unwrap();
  var pure15 = /* @__PURE__ */ pure(applicativeIdentity);
  var whole3 = /* @__PURE__ */ whole2(typedArrayUint8);
  var part2 = /* @__PURE__ */ part(typedArrayUint8)(bytesPerTypeUint8);
  var Buff = /* @__PURE__ */ function() {
    function Buff2(value0) {
      this.value0 = value0;
    }
    ;
    Buff2.create = function(value0) {
      return new Buff2(value0);
    };
    return Buff2;
  }();
  var View = /* @__PURE__ */ function() {
    function View2(value0) {
      this.value0 = value0;
    }
    ;
    View2.create = function(value0) {
      return new View2(value0);
    };
    return View2;
  }();
  var Node = /* @__PURE__ */ function() {
    function Node2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    Node2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new Node2(value0, value1, value22);
        };
      };
    };
    return Node2;
  }();
  var Null = /* @__PURE__ */ function() {
    function Null2() {
    }
    ;
    Null2.value = new Null2();
    return Null2;
  }();
  var singleton9 = function(buf) {
    return new Node(Null.value, buf, Null.value);
  };
  var semigroupBuilder = {
    append: function(v) {
      return function(v1) {
        if (v instanceof Null && v1 instanceof Null) {
          return Null.value;
        }
        ;
        if (v1 instanceof Null) {
          return v;
        }
        ;
        if (v instanceof Null) {
          return v1;
        }
        ;
        if (v1 instanceof Node && v1.value0 instanceof Null) {
          return new Node(v, v1.value1, v1.value2);
        }
        ;
        if (v instanceof Node && v.value2 instanceof Null) {
          return new Node(v.value0, v.value1, v1);
        }
        ;
        if (v instanceof Node) {
          return new Node(v.value0, v.value1, append(semigroupBuilder)(v.value2)(v1));
        }
        ;
        throw new Error("Failed pattern match at Data.ArrayBuffer.Builder.Internal (line 187, column 1 - line 194, column 54): " + [v.constructor.name, v1.constructor.name]);
      };
    }
  };
  var monoidBuilder = /* @__PURE__ */ function() {
    return {
      mempty: Null.value,
      Semigroup0: function() {
        return semigroupBuilder;
      }
    };
  }();
  var foldM4 = function(dictMonadRec) {
    var Monad0 = dictMonadRec.Monad0();
    var pure110 = pure(Monad0.Applicative0());
    var bind20 = bind(Monad0.Bind1());
    var tailRecM5 = tailRecM(dictMonadRec);
    return function(f) {
      return function(a0) {
        return function(b2) {
          var inner = function(v) {
            if (v.p_ instanceof Null) {
              return pure110(new Done(v.stack_));
            }
            ;
            if (v.p_ instanceof Node) {
              return pure110(new Loop({
                p_: v.p_.value0,
                stack_: new Cons(v.p_, v.stack_)
              }));
            }
            ;
            throw new Error("Failed pattern match at Data.ArrayBuffer.Builder.Internal (line 224, column 3 - line 224, column 50): " + [v.constructor.name]);
          };
          var outer = function(v) {
            return bind20(tailRecM5(inner)({
              p_: v.p,
              stack_: v.stack
            }))(function(stack$prime) {
              var v1 = uncons(stack$prime);
              if (v1 instanceof Nothing) {
                return pure110(new Done(v.accum));
              }
              ;
              if (v1 instanceof Just && v1.value0.head instanceof Node) {
                return bind20(f(v.accum)(v1.value0.head.value1))(function(accum$prime) {
                  return pure110(new Loop({
                    p: v1.value0.head.value2,
                    stack: v1.value0.tail,
                    accum: accum$prime
                  }));
                });
              }
              ;
              if (v1 instanceof Just && v1.value0.head instanceof Null) {
                return pure110(new Done(v.accum));
              }
              ;
              throw new Error("Failed pattern match at Data.ArrayBuffer.Builder.Internal (line 218, column 5 - line 223, column 47): " + [v1.constructor.name]);
            });
          };
          return tailRecM5(outer)({
            p: b2,
            stack: Nil.value,
            accum: a0
          });
        };
      };
    };
  };
  var foldM1 = /* @__PURE__ */ foldM4(monadRecIdentity);
  var foldl2 = function(f) {
    return function(a0) {
      return function(b2) {
        return unwrap5(foldM1(function(a$prime) {
          return function(b$prime) {
            return pure15(f(a$prime)(b$prime));
          };
        })(a0)(b2));
      };
    };
  };
  var length11 = function(bldr) {
    var len = function(v) {
      if (v instanceof Buff) {
        return byteLength(v.value0);
      }
      ;
      if (v instanceof View) {
        return byteLength2(v.value0);
      }
      ;
      throw new Error("Failed pattern match at Data.ArrayBuffer.Builder.Internal (line 236, column 3 - line 236, column 35): " + [v.constructor.name]);
    };
    return foldl2(function(b2) {
      return function(a2) {
        return b2 + len(a2) | 0;
      };
    })(0)(bldr);
  };
  var execBuilder = function(dictMonadEffect) {
    var Monad0 = dictMonadEffect.Monad0();
    var bind20 = bind(Monad0.Bind1());
    var liftEffect10 = liftEffect(dictMonadEffect);
    var pure110 = pure(Monad0.Applicative0());
    return function(dictMonadRec) {
      var foldM22 = foldM4(dictMonadRec);
      return function(bldr) {
        var toUint8Array = function(v) {
          if (v instanceof Buff) {
            return whole3(v.value0);
          }
          ;
          if (v instanceof View) {
            return part2(buffer(v.value0))(byteOffset(v.value0))(byteLength2(v.value0));
          }
          ;
          throw new Error("Failed pattern match at Data.ArrayBuffer.Builder.Internal (line 267, column 3 - line 267, column 48): " + [v.constructor.name]);
        };
        var buflen = length11(bldr);
        return bind20(liftEffect10(empty7(buflen)))(function(buf) {
          return bind20(liftEffect10(whole3(buf)))(function(newview) {
            return bind20(foldM22(function(offset) {
              return function(a2) {
                return bind20(liftEffect10(toUint8Array(a2)))(function(aview) {
                  return bind20(liftEffect10(setTyped(newview)(new Just(offset))(aview)))(function() {
                    return pure110(offset + byteLength3(aview) | 0);
                  });
                });
              };
            })(0)(bldr))(function() {
              return pure110(buf);
            });
          });
        });
      };
    };
  };
  var encodeUint8 = function(dictMonadEffect) {
    var Monad0 = dictMonadEffect.Monad0();
    var bind20 = bind(Monad0.Bind1());
    var liftEffect10 = liftEffect(dictMonadEffect);
    var pure110 = pure(Monad0.Applicative0());
    return function(x) {
      return bind20(liftEffect10(empty7(1)))(function(buf) {
        return bind20(liftEffect10(setUint8(whole(buf))(0)(x)))(function() {
          return pure110(buf);
        });
      });
    };
  };

  // output/Data.ArrayBuffer.Builder/index.js
  var monadTellWriterT2 = /* @__PURE__ */ monadTellWriterT(monoidBuilder);
  var bindWriterT2 = /* @__PURE__ */ bindWriterT(semigroupBuilder);
  var monadEffectWriter2 = /* @__PURE__ */ monadEffectWriter(monoidBuilder);
  var putArrayBuffer = function(dictMonad) {
    var $152 = tell(monadTellWriterT2(dictMonad));
    return function($153) {
      return $152(singleton9(Buff.create($153)));
    };
  };
  var putUint8 = function(dictMonadEffect) {
    var Monad0 = dictMonadEffect.Monad0();
    return composeKleisliFlipped(bindWriterT2(Monad0.Bind1()))(putArrayBuffer(Monad0))(encodeUint8(monadEffectWriter2(dictMonadEffect)));
  };
  var execPutM = function(dictMonadEffect) {
    var Bind1 = dictMonadEffect.Monad0().Bind1();
    var composeKleisliFlipped4 = composeKleisliFlipped(Bind1);
    var execBuilder2 = execBuilder(dictMonadEffect);
    var execWriterT2 = execWriterT(Bind1.Apply0().Functor0());
    return function(dictMonadRec) {
      return composeKleisliFlipped4(execBuilder2(dictMonadRec))(execWriterT2);
    };
  };
  var execPut = /* @__PURE__ */ execPutM(monadEffectEffect)(monadRecEffect);

  // output/Data.DateTime.Instant/foreign.js
  var createDateTime = function(y, m, d, h, mi, s, ms) {
    var dateTime3 = new Date(Date.UTC(y, m, d, h, mi, s, ms));
    if (y >= 0 && y < 100) {
      dateTime3.setUTCFullYear(y);
    }
    return dateTime3;
  };
  function fromDateTimeImpl(y, mo, d, h, mi, s, ms) {
    return createDateTime(y, mo - 1, d, h, mi, s, ms).getTime();
  }

  // output/Data.Date/foreign.js
  var createDate = function(y, m, d) {
    var date2 = new Date(Date.UTC(y, m, d));
    if (y >= 0 && y < 100) {
      date2.setUTCFullYear(y);
    }
    return date2;
  };
  function canonicalDateImpl(ctor, y, m, d) {
    var date2 = createDate(y, m - 1, d);
    return ctor(date2.getUTCFullYear())(date2.getUTCMonth() + 1)(date2.getUTCDate());
  }
  function calcWeekday(y, m, d) {
    return createDate(y, m - 1, d).getUTCDay();
  }

  // output/Data.Date.Component/index.js
  var $runtime_lazy9 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var Monday = /* @__PURE__ */ function() {
    function Monday2() {
    }
    ;
    Monday2.value = new Monday2();
    return Monday2;
  }();
  var Tuesday = /* @__PURE__ */ function() {
    function Tuesday2() {
    }
    ;
    Tuesday2.value = new Tuesday2();
    return Tuesday2;
  }();
  var Wednesday = /* @__PURE__ */ function() {
    function Wednesday2() {
    }
    ;
    Wednesday2.value = new Wednesday2();
    return Wednesday2;
  }();
  var Thursday = /* @__PURE__ */ function() {
    function Thursday2() {
    }
    ;
    Thursday2.value = new Thursday2();
    return Thursday2;
  }();
  var Friday = /* @__PURE__ */ function() {
    function Friday2() {
    }
    ;
    Friday2.value = new Friday2();
    return Friday2;
  }();
  var Saturday = /* @__PURE__ */ function() {
    function Saturday2() {
    }
    ;
    Saturday2.value = new Saturday2();
    return Saturday2;
  }();
  var Sunday = /* @__PURE__ */ function() {
    function Sunday2() {
    }
    ;
    Sunday2.value = new Sunday2();
    return Sunday2;
  }();
  var January = /* @__PURE__ */ function() {
    function January2() {
    }
    ;
    January2.value = new January2();
    return January2;
  }();
  var February = /* @__PURE__ */ function() {
    function February2() {
    }
    ;
    February2.value = new February2();
    return February2;
  }();
  var March = /* @__PURE__ */ function() {
    function March2() {
    }
    ;
    March2.value = new March2();
    return March2;
  }();
  var April = /* @__PURE__ */ function() {
    function April2() {
    }
    ;
    April2.value = new April2();
    return April2;
  }();
  var May = /* @__PURE__ */ function() {
    function May2() {
    }
    ;
    May2.value = new May2();
    return May2;
  }();
  var June = /* @__PURE__ */ function() {
    function June2() {
    }
    ;
    June2.value = new June2();
    return June2;
  }();
  var July = /* @__PURE__ */ function() {
    function July2() {
    }
    ;
    July2.value = new July2();
    return July2;
  }();
  var August = /* @__PURE__ */ function() {
    function August2() {
    }
    ;
    August2.value = new August2();
    return August2;
  }();
  var September = /* @__PURE__ */ function() {
    function September2() {
    }
    ;
    September2.value = new September2();
    return September2;
  }();
  var October = /* @__PURE__ */ function() {
    function October2() {
    }
    ;
    October2.value = new October2();
    return October2;
  }();
  var November = /* @__PURE__ */ function() {
    function November2() {
    }
    ;
    November2.value = new November2();
    return November2;
  }();
  var December = /* @__PURE__ */ function() {
    function December2() {
    }
    ;
    December2.value = new December2();
    return December2;
  }();
  var showWeekday = {
    show: function(v) {
      if (v instanceof Monday) {
        return "Monday";
      }
      ;
      if (v instanceof Tuesday) {
        return "Tuesday";
      }
      ;
      if (v instanceof Wednesday) {
        return "Wednesday";
      }
      ;
      if (v instanceof Thursday) {
        return "Thursday";
      }
      ;
      if (v instanceof Friday) {
        return "Friday";
      }
      ;
      if (v instanceof Saturday) {
        return "Saturday";
      }
      ;
      if (v instanceof Sunday) {
        return "Sunday";
      }
      ;
      throw new Error("Failed pattern match at Data.Date.Component (line 184, column 1 - line 191, column 25): " + [v.constructor.name]);
    }
  };
  var showMonth = {
    show: function(v) {
      if (v instanceof January) {
        return "January";
      }
      ;
      if (v instanceof February) {
        return "February";
      }
      ;
      if (v instanceof March) {
        return "March";
      }
      ;
      if (v instanceof April) {
        return "April";
      }
      ;
      if (v instanceof May) {
        return "May";
      }
      ;
      if (v instanceof June) {
        return "June";
      }
      ;
      if (v instanceof July) {
        return "July";
      }
      ;
      if (v instanceof August) {
        return "August";
      }
      ;
      if (v instanceof September) {
        return "September";
      }
      ;
      if (v instanceof October) {
        return "October";
      }
      ;
      if (v instanceof November) {
        return "November";
      }
      ;
      if (v instanceof December) {
        return "December";
      }
      ;
      throw new Error("Failed pattern match at Data.Date.Component (line 101, column 1 - line 113, column 29): " + [v.constructor.name]);
    }
  };
  var ordYear = ordInt;
  var ordDay = ordInt;
  var eqWeekday = {
    eq: function(x) {
      return function(y) {
        if (x instanceof Monday && y instanceof Monday) {
          return true;
        }
        ;
        if (x instanceof Tuesday && y instanceof Tuesday) {
          return true;
        }
        ;
        if (x instanceof Wednesday && y instanceof Wednesday) {
          return true;
        }
        ;
        if (x instanceof Thursday && y instanceof Thursday) {
          return true;
        }
        ;
        if (x instanceof Friday && y instanceof Friday) {
          return true;
        }
        ;
        if (x instanceof Saturday && y instanceof Saturday) {
          return true;
        }
        ;
        if (x instanceof Sunday && y instanceof Sunday) {
          return true;
        }
        ;
        return false;
      };
    }
  };
  var ordWeekday = {
    compare: function(x) {
      return function(y) {
        if (x instanceof Monday && y instanceof Monday) {
          return EQ.value;
        }
        ;
        if (x instanceof Monday) {
          return LT.value;
        }
        ;
        if (y instanceof Monday) {
          return GT.value;
        }
        ;
        if (x instanceof Tuesday && y instanceof Tuesday) {
          return EQ.value;
        }
        ;
        if (x instanceof Tuesday) {
          return LT.value;
        }
        ;
        if (y instanceof Tuesday) {
          return GT.value;
        }
        ;
        if (x instanceof Wednesday && y instanceof Wednesday) {
          return EQ.value;
        }
        ;
        if (x instanceof Wednesday) {
          return LT.value;
        }
        ;
        if (y instanceof Wednesday) {
          return GT.value;
        }
        ;
        if (x instanceof Thursday && y instanceof Thursday) {
          return EQ.value;
        }
        ;
        if (x instanceof Thursday) {
          return LT.value;
        }
        ;
        if (y instanceof Thursday) {
          return GT.value;
        }
        ;
        if (x instanceof Friday && y instanceof Friday) {
          return EQ.value;
        }
        ;
        if (x instanceof Friday) {
          return LT.value;
        }
        ;
        if (y instanceof Friday) {
          return GT.value;
        }
        ;
        if (x instanceof Saturday && y instanceof Saturday) {
          return EQ.value;
        }
        ;
        if (x instanceof Saturday) {
          return LT.value;
        }
        ;
        if (y instanceof Saturday) {
          return GT.value;
        }
        ;
        if (x instanceof Sunday && y instanceof Sunday) {
          return EQ.value;
        }
        ;
        throw new Error("Failed pattern match at Data.Date.Component (line 0, column 0 - line 0, column 0): " + [x.constructor.name, y.constructor.name]);
      };
    },
    Eq0: function() {
      return eqWeekday;
    }
  };
  var eqMonth = {
    eq: function(x) {
      return function(y) {
        if (x instanceof January && y instanceof January) {
          return true;
        }
        ;
        if (x instanceof February && y instanceof February) {
          return true;
        }
        ;
        if (x instanceof March && y instanceof March) {
          return true;
        }
        ;
        if (x instanceof April && y instanceof April) {
          return true;
        }
        ;
        if (x instanceof May && y instanceof May) {
          return true;
        }
        ;
        if (x instanceof June && y instanceof June) {
          return true;
        }
        ;
        if (x instanceof July && y instanceof July) {
          return true;
        }
        ;
        if (x instanceof August && y instanceof August) {
          return true;
        }
        ;
        if (x instanceof September && y instanceof September) {
          return true;
        }
        ;
        if (x instanceof October && y instanceof October) {
          return true;
        }
        ;
        if (x instanceof November && y instanceof November) {
          return true;
        }
        ;
        if (x instanceof December && y instanceof December) {
          return true;
        }
        ;
        return false;
      };
    }
  };
  var ordMonth = {
    compare: function(x) {
      return function(y) {
        if (x instanceof January && y instanceof January) {
          return EQ.value;
        }
        ;
        if (x instanceof January) {
          return LT.value;
        }
        ;
        if (y instanceof January) {
          return GT.value;
        }
        ;
        if (x instanceof February && y instanceof February) {
          return EQ.value;
        }
        ;
        if (x instanceof February) {
          return LT.value;
        }
        ;
        if (y instanceof February) {
          return GT.value;
        }
        ;
        if (x instanceof March && y instanceof March) {
          return EQ.value;
        }
        ;
        if (x instanceof March) {
          return LT.value;
        }
        ;
        if (y instanceof March) {
          return GT.value;
        }
        ;
        if (x instanceof April && y instanceof April) {
          return EQ.value;
        }
        ;
        if (x instanceof April) {
          return LT.value;
        }
        ;
        if (y instanceof April) {
          return GT.value;
        }
        ;
        if (x instanceof May && y instanceof May) {
          return EQ.value;
        }
        ;
        if (x instanceof May) {
          return LT.value;
        }
        ;
        if (y instanceof May) {
          return GT.value;
        }
        ;
        if (x instanceof June && y instanceof June) {
          return EQ.value;
        }
        ;
        if (x instanceof June) {
          return LT.value;
        }
        ;
        if (y instanceof June) {
          return GT.value;
        }
        ;
        if (x instanceof July && y instanceof July) {
          return EQ.value;
        }
        ;
        if (x instanceof July) {
          return LT.value;
        }
        ;
        if (y instanceof July) {
          return GT.value;
        }
        ;
        if (x instanceof August && y instanceof August) {
          return EQ.value;
        }
        ;
        if (x instanceof August) {
          return LT.value;
        }
        ;
        if (y instanceof August) {
          return GT.value;
        }
        ;
        if (x instanceof September && y instanceof September) {
          return EQ.value;
        }
        ;
        if (x instanceof September) {
          return LT.value;
        }
        ;
        if (y instanceof September) {
          return GT.value;
        }
        ;
        if (x instanceof October && y instanceof October) {
          return EQ.value;
        }
        ;
        if (x instanceof October) {
          return LT.value;
        }
        ;
        if (y instanceof October) {
          return GT.value;
        }
        ;
        if (x instanceof November && y instanceof November) {
          return EQ.value;
        }
        ;
        if (x instanceof November) {
          return LT.value;
        }
        ;
        if (y instanceof November) {
          return GT.value;
        }
        ;
        if (x instanceof December && y instanceof December) {
          return EQ.value;
        }
        ;
        throw new Error("Failed pattern match at Data.Date.Component (line 0, column 0 - line 0, column 0): " + [x.constructor.name, y.constructor.name]);
      };
    },
    Eq0: function() {
      return eqMonth;
    }
  };
  var boundedYear = /* @__PURE__ */ function() {
    return {
      bottom: -271820 | 0,
      top: 275759,
      Ord0: function() {
        return ordYear;
      }
    };
  }();
  var boundedWeekday = /* @__PURE__ */ function() {
    return {
      bottom: Monday.value,
      top: Sunday.value,
      Ord0: function() {
        return ordWeekday;
      }
    };
  }();
  var boundedMonth = /* @__PURE__ */ function() {
    return {
      bottom: January.value,
      top: December.value,
      Ord0: function() {
        return ordMonth;
      }
    };
  }();
  var boundedEnumYear = {
    cardinality: 547580,
    toEnum: function(n) {
      if (n >= (-271820 | 0) && n <= 275759) {
        return new Just(n);
      }
      ;
      if (otherwise) {
        return Nothing.value;
      }
      ;
      throw new Error("Failed pattern match at Data.Date.Component (line 35, column 1 - line 40, column 24): " + [n.constructor.name]);
    },
    fromEnum: function(v) {
      return v;
    },
    Bounded0: function() {
      return boundedYear;
    },
    Enum1: function() {
      return $lazy_enumYear(0);
    }
  };
  var $lazy_enumYear = /* @__PURE__ */ $runtime_lazy9("enumYear", "Data.Date.Component", function() {
    return {
      succ: function() {
        var $55 = toEnum(boundedEnumYear);
        var $56 = fromEnum(boundedEnumYear);
        return function($57) {
          return $55(function(v) {
            return v + 1 | 0;
          }($56($57)));
        };
      }(),
      pred: function() {
        var $58 = toEnum(boundedEnumYear);
        var $59 = fromEnum(boundedEnumYear);
        return function($60) {
          return $58(function(v) {
            return v - 1 | 0;
          }($59($60)));
        };
      }(),
      Ord0: function() {
        return ordYear;
      }
    };
  });
  var boundedEnumWeekday = {
    cardinality: 7,
    toEnum: function(v) {
      if (v === 1) {
        return new Just(Monday.value);
      }
      ;
      if (v === 2) {
        return new Just(Tuesday.value);
      }
      ;
      if (v === 3) {
        return new Just(Wednesday.value);
      }
      ;
      if (v === 4) {
        return new Just(Thursday.value);
      }
      ;
      if (v === 5) {
        return new Just(Friday.value);
      }
      ;
      if (v === 6) {
        return new Just(Saturday.value);
      }
      ;
      if (v === 7) {
        return new Just(Sunday.value);
      }
      ;
      return Nothing.value;
    },
    fromEnum: function(v) {
      if (v instanceof Monday) {
        return 1;
      }
      ;
      if (v instanceof Tuesday) {
        return 2;
      }
      ;
      if (v instanceof Wednesday) {
        return 3;
      }
      ;
      if (v instanceof Thursday) {
        return 4;
      }
      ;
      if (v instanceof Friday) {
        return 5;
      }
      ;
      if (v instanceof Saturday) {
        return 6;
      }
      ;
      if (v instanceof Sunday) {
        return 7;
      }
      ;
      throw new Error("Failed pattern match at Data.Date.Component (line 175, column 14 - line 182, column 16): " + [v.constructor.name]);
    },
    Bounded0: function() {
      return boundedWeekday;
    },
    Enum1: function() {
      return $lazy_enumWeekday(0);
    }
  };
  var $lazy_enumWeekday = /* @__PURE__ */ $runtime_lazy9("enumWeekday", "Data.Date.Component", function() {
    return {
      succ: function() {
        var $61 = toEnum(boundedEnumWeekday);
        var $62 = fromEnum(boundedEnumWeekday);
        return function($63) {
          return $61(function(v) {
            return v + 1 | 0;
          }($62($63)));
        };
      }(),
      pred: function() {
        var $64 = toEnum(boundedEnumWeekday);
        var $65 = fromEnum(boundedEnumWeekday);
        return function($66) {
          return $64(function(v) {
            return v - 1 | 0;
          }($65($66)));
        };
      }(),
      Ord0: function() {
        return ordWeekday;
      }
    };
  });
  var boundedEnumMonth = {
    cardinality: 12,
    toEnum: function(v) {
      if (v === 1) {
        return new Just(January.value);
      }
      ;
      if (v === 2) {
        return new Just(February.value);
      }
      ;
      if (v === 3) {
        return new Just(March.value);
      }
      ;
      if (v === 4) {
        return new Just(April.value);
      }
      ;
      if (v === 5) {
        return new Just(May.value);
      }
      ;
      if (v === 6) {
        return new Just(June.value);
      }
      ;
      if (v === 7) {
        return new Just(July.value);
      }
      ;
      if (v === 8) {
        return new Just(August.value);
      }
      ;
      if (v === 9) {
        return new Just(September.value);
      }
      ;
      if (v === 10) {
        return new Just(October.value);
      }
      ;
      if (v === 11) {
        return new Just(November.value);
      }
      ;
      if (v === 12) {
        return new Just(December.value);
      }
      ;
      return Nothing.value;
    },
    fromEnum: function(v) {
      if (v instanceof January) {
        return 1;
      }
      ;
      if (v instanceof February) {
        return 2;
      }
      ;
      if (v instanceof March) {
        return 3;
      }
      ;
      if (v instanceof April) {
        return 4;
      }
      ;
      if (v instanceof May) {
        return 5;
      }
      ;
      if (v instanceof June) {
        return 6;
      }
      ;
      if (v instanceof July) {
        return 7;
      }
      ;
      if (v instanceof August) {
        return 8;
      }
      ;
      if (v instanceof September) {
        return 9;
      }
      ;
      if (v instanceof October) {
        return 10;
      }
      ;
      if (v instanceof November) {
        return 11;
      }
      ;
      if (v instanceof December) {
        return 12;
      }
      ;
      throw new Error("Failed pattern match at Data.Date.Component (line 87, column 14 - line 99, column 19): " + [v.constructor.name]);
    },
    Bounded0: function() {
      return boundedMonth;
    },
    Enum1: function() {
      return $lazy_enumMonth(0);
    }
  };
  var $lazy_enumMonth = /* @__PURE__ */ $runtime_lazy9("enumMonth", "Data.Date.Component", function() {
    return {
      succ: function() {
        var $67 = toEnum(boundedEnumMonth);
        var $68 = fromEnum(boundedEnumMonth);
        return function($69) {
          return $67(function(v) {
            return v + 1 | 0;
          }($68($69)));
        };
      }(),
      pred: function() {
        var $70 = toEnum(boundedEnumMonth);
        var $71 = fromEnum(boundedEnumMonth);
        return function($72) {
          return $70(function(v) {
            return v - 1 | 0;
          }($71($72)));
        };
      }(),
      Ord0: function() {
        return ordMonth;
      }
    };
  });
  var boundedDay = {
    bottom: 1,
    top: 31,
    Ord0: function() {
      return ordDay;
    }
  };
  var boundedEnumDay = {
    cardinality: 31,
    toEnum: function(n) {
      if (n >= 1 && n <= 31) {
        return new Just(n);
      }
      ;
      if (otherwise) {
        return Nothing.value;
      }
      ;
      throw new Error("Failed pattern match at Data.Date.Component (line 133, column 1 - line 138, column 23): " + [n.constructor.name]);
    },
    fromEnum: function(v) {
      return v;
    },
    Bounded0: function() {
      return boundedDay;
    },
    Enum1: function() {
      return $lazy_enumDay(0);
    }
  };
  var $lazy_enumDay = /* @__PURE__ */ $runtime_lazy9("enumDay", "Data.Date.Component", function() {
    return {
      succ: function() {
        var $73 = toEnum(boundedEnumDay);
        var $74 = fromEnum(boundedEnumDay);
        return function($75) {
          return $73(function(v) {
            return v + 1 | 0;
          }($74($75)));
        };
      }(),
      pred: function() {
        var $76 = toEnum(boundedEnumDay);
        var $77 = fromEnum(boundedEnumDay);
        return function($78) {
          return $76(function(v) {
            return v - 1 | 0;
          }($77($78)));
        };
      }(),
      Ord0: function() {
        return ordDay;
      }
    };
  });

  // output/Data.Date/index.js
  var fromEnum5 = /* @__PURE__ */ fromEnum(boundedEnumMonth);
  var fromJust6 = /* @__PURE__ */ fromJust();
  var toEnum3 = /* @__PURE__ */ toEnum(boundedEnumWeekday);
  var toEnum22 = /* @__PURE__ */ toEnum(boundedEnumMonth);
  var $$Date = /* @__PURE__ */ function() {
    function $$Date2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    $$Date2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new $$Date2(value0, value1, value22);
        };
      };
    };
    return $$Date2;
  }();
  var year = function(v) {
    return v.value0;
  };
  var weekday = function(v) {
    var n = calcWeekday(v.value0, fromEnum5(v.value1), v.value2);
    var $86 = n === 0;
    if ($86) {
      return fromJust6(toEnum3(7));
    }
    ;
    return fromJust6(toEnum3(n));
  };
  var month = function(v) {
    return v.value1;
  };
  var day = function(v) {
    return v.value2;
  };
  var canonicalDate = function(y) {
    return function(m) {
      return function(d) {
        var mkDate = function(y$prime) {
          return function(m$prime) {
            return function(d$prime) {
              return new $$Date(y$prime, fromJust6(toEnum22(m$prime)), d$prime);
            };
          };
        };
        return canonicalDateImpl(mkDate, y, fromEnum5(m), d);
      };
    };
  };

  // output/Data.Time.Component/index.js
  var $runtime_lazy10 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var ordSecond = ordInt;
  var ordMinute = ordInt;
  var ordMillisecond = ordInt;
  var ordHour = ordInt;
  var boundedSecond = {
    bottom: 0,
    top: 59,
    Ord0: function() {
      return ordSecond;
    }
  };
  var boundedMinute = {
    bottom: 0,
    top: 59,
    Ord0: function() {
      return ordMinute;
    }
  };
  var boundedMillisecond = {
    bottom: 0,
    top: 999,
    Ord0: function() {
      return ordMillisecond;
    }
  };
  var boundedHour = {
    bottom: 0,
    top: 23,
    Ord0: function() {
      return ordHour;
    }
  };
  var boundedEnumSecond = {
    cardinality: 60,
    toEnum: function(n) {
      if (n >= 0 && n <= 59) {
        return new Just(n);
      }
      ;
      if (otherwise) {
        return Nothing.value;
      }
      ;
      throw new Error("Failed pattern match at Data.Time.Component (line 90, column 1 - line 95, column 26): " + [n.constructor.name]);
    },
    fromEnum: function(v) {
      return v;
    },
    Bounded0: function() {
      return boundedSecond;
    },
    Enum1: function() {
      return $lazy_enumSecond(0);
    }
  };
  var $lazy_enumSecond = /* @__PURE__ */ $runtime_lazy10("enumSecond", "Data.Time.Component", function() {
    return {
      succ: function() {
        var $36 = toEnum(boundedEnumSecond);
        var $37 = fromEnum(boundedEnumSecond);
        return function($38) {
          return $36(function(v) {
            return v + 1 | 0;
          }($37($38)));
        };
      }(),
      pred: function() {
        var $39 = toEnum(boundedEnumSecond);
        var $40 = fromEnum(boundedEnumSecond);
        return function($41) {
          return $39(function(v) {
            return v - 1 | 0;
          }($40($41)));
        };
      }(),
      Ord0: function() {
        return ordSecond;
      }
    };
  });
  var boundedEnumMinute = {
    cardinality: 60,
    toEnum: function(n) {
      if (n >= 0 && n <= 59) {
        return new Just(n);
      }
      ;
      if (otherwise) {
        return Nothing.value;
      }
      ;
      throw new Error("Failed pattern match at Data.Time.Component (line 61, column 1 - line 66, column 26): " + [n.constructor.name]);
    },
    fromEnum: function(v) {
      return v;
    },
    Bounded0: function() {
      return boundedMinute;
    },
    Enum1: function() {
      return $lazy_enumMinute(0);
    }
  };
  var $lazy_enumMinute = /* @__PURE__ */ $runtime_lazy10("enumMinute", "Data.Time.Component", function() {
    return {
      succ: function() {
        var $42 = toEnum(boundedEnumMinute);
        var $43 = fromEnum(boundedEnumMinute);
        return function($44) {
          return $42(function(v) {
            return v + 1 | 0;
          }($43($44)));
        };
      }(),
      pred: function() {
        var $45 = toEnum(boundedEnumMinute);
        var $46 = fromEnum(boundedEnumMinute);
        return function($47) {
          return $45(function(v) {
            return v - 1 | 0;
          }($46($47)));
        };
      }(),
      Ord0: function() {
        return ordMinute;
      }
    };
  });
  var boundedEnumMillisecond = {
    cardinality: 1e3,
    toEnum: function(n) {
      if (n >= 0 && n <= 999) {
        return new Just(n);
      }
      ;
      if (otherwise) {
        return Nothing.value;
      }
      ;
      throw new Error("Failed pattern match at Data.Time.Component (line 120, column 1 - line 125, column 31): " + [n.constructor.name]);
    },
    fromEnum: function(v) {
      return v;
    },
    Bounded0: function() {
      return boundedMillisecond;
    },
    Enum1: function() {
      return $lazy_enumMillisecond(0);
    }
  };
  var $lazy_enumMillisecond = /* @__PURE__ */ $runtime_lazy10("enumMillisecond", "Data.Time.Component", function() {
    return {
      succ: function() {
        var $48 = toEnum(boundedEnumMillisecond);
        var $49 = fromEnum(boundedEnumMillisecond);
        return function($50) {
          return $48(function(v) {
            return v + 1 | 0;
          }($49($50)));
        };
      }(),
      pred: function() {
        var $51 = toEnum(boundedEnumMillisecond);
        var $52 = fromEnum(boundedEnumMillisecond);
        return function($53) {
          return $51(function(v) {
            return v - 1 | 0;
          }($52($53)));
        };
      }(),
      Ord0: function() {
        return ordMillisecond;
      }
    };
  });
  var boundedEnumHour = {
    cardinality: 24,
    toEnum: function(n) {
      if (n >= 0 && n <= 23) {
        return new Just(n);
      }
      ;
      if (otherwise) {
        return Nothing.value;
      }
      ;
      throw new Error("Failed pattern match at Data.Time.Component (line 32, column 1 - line 37, column 24): " + [n.constructor.name]);
    },
    fromEnum: function(v) {
      return v;
    },
    Bounded0: function() {
      return boundedHour;
    },
    Enum1: function() {
      return $lazy_enumHour(0);
    }
  };
  var $lazy_enumHour = /* @__PURE__ */ $runtime_lazy10("enumHour", "Data.Time.Component", function() {
    return {
      succ: function() {
        var $54 = toEnum(boundedEnumHour);
        var $55 = fromEnum(boundedEnumHour);
        return function($56) {
          return $54(function(v) {
            return v + 1 | 0;
          }($55($56)));
        };
      }(),
      pred: function() {
        var $57 = toEnum(boundedEnumHour);
        var $58 = fromEnum(boundedEnumHour);
        return function($59) {
          return $57(function(v) {
            return v - 1 | 0;
          }($58($59)));
        };
      }(),
      Ord0: function() {
        return ordHour;
      }
    };
  });

  // output/Data.Time/index.js
  var Time = /* @__PURE__ */ function() {
    function Time2(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    Time2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new Time2(value0, value1, value22, value32);
          };
        };
      };
    };
    return Time2;
  }();
  var second = function(v) {
    return v.value2;
  };
  var minute = function(v) {
    return v.value1;
  };
  var millisecond = function(v) {
    return v.value3;
  };
  var hour = function(v) {
    return v.value0;
  };

  // output/Data.DateTime/index.js
  var DateTime = /* @__PURE__ */ function() {
    function DateTime2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    DateTime2.create = function(value0) {
      return function(value1) {
        return new DateTime2(value0, value1);
      };
    };
    return DateTime2;
  }();

  // output/Data.DateTime.Instant/index.js
  var fromEnum6 = /* @__PURE__ */ fromEnum(boundedEnumMonth);
  var append12 = /* @__PURE__ */ append(semigroupMilliseconds);
  var negateDuration2 = /* @__PURE__ */ negateDuration(durationMilliseconds);
  var unInstant = function(v) {
    return v;
  };
  var fromDateTime = function(v) {
    return fromDateTimeImpl(year(v.value0), fromEnum6(month(v.value0)), day(v.value0), hour(v.value1), minute(v.value1), second(v.value1), millisecond(v.value1));
  };
  var eqDateTime = eqMilliseconds;
  var diff = function(dictDuration) {
    var toDuration2 = toDuration(dictDuration);
    return function(dt1) {
      return function(dt2) {
        return toDuration2(append12(unInstant(dt1))(negateDuration2(unInstant(dt2))));
      };
    };
  };

  // output/Data.Number.Format/foreign.js
  function wrap3(method2) {
    return function(d) {
      return function(num) {
        return method2.apply(num, [d]);
      };
    };
  }
  var toPrecisionNative = wrap3(Number.prototype.toPrecision);
  var toFixedNative = wrap3(Number.prototype.toFixed);
  var toExponentialNative = wrap3(Number.prototype.toExponential);
  function toString2(num) {
    return num.toString();
  }

  // output/Data.Number.Format/index.js
  var clamp2 = /* @__PURE__ */ clamp(ordInt);
  var Precision = /* @__PURE__ */ function() {
    function Precision2(value0) {
      this.value0 = value0;
    }
    ;
    Precision2.create = function(value0) {
      return new Precision2(value0);
    };
    return Precision2;
  }();
  var Fixed = /* @__PURE__ */ function() {
    function Fixed2(value0) {
      this.value0 = value0;
    }
    ;
    Fixed2.create = function(value0) {
      return new Fixed2(value0);
    };
    return Fixed2;
  }();
  var Exponential = /* @__PURE__ */ function() {
    function Exponential2(value0) {
      this.value0 = value0;
    }
    ;
    Exponential2.create = function(value0) {
      return new Exponential2(value0);
    };
    return Exponential2;
  }();
  var toStringWith = function(v) {
    if (v instanceof Precision) {
      return toPrecisionNative(v.value0);
    }
    ;
    if (v instanceof Fixed) {
      return toFixedNative(v.value0);
    }
    ;
    if (v instanceof Exponential) {
      return toExponentialNative(v.value0);
    }
    ;
    throw new Error("Failed pattern match at Data.Number.Format (line 59, column 1 - line 59, column 43): " + [v.constructor.name]);
  };
  var fixed = /* @__PURE__ */ function() {
    var $9 = clamp2(0)(20);
    return function($10) {
      return Fixed.create($9($10));
    };
  }();

  // output/Data.Formatter.DateTime/index.js
  var show6 = /* @__PURE__ */ show(showInt);
  var foldMap2 = /* @__PURE__ */ foldMap(foldableList);
  var foldMap12 = /* @__PURE__ */ foldMap2(monoidString);
  var abs3 = /* @__PURE__ */ abs(ordInt)(ringInt);
  var fromEnum7 = /* @__PURE__ */ fromEnum(boundedEnumYear);
  var show13 = /* @__PURE__ */ show(showMonth);
  var fromEnum1 = /* @__PURE__ */ fromEnum(boundedEnumMonth);
  var fromEnum22 = /* @__PURE__ */ fromEnum(boundedEnumDay);
  var unwrap6 = /* @__PURE__ */ unwrap();
  var fromEnum32 = /* @__PURE__ */ fromEnum(boundedEnumWeekday);
  var show24 = /* @__PURE__ */ show(showWeekday);
  var fromEnum42 = /* @__PURE__ */ fromEnum(boundedEnumHour);
  var mod4 = /* @__PURE__ */ mod(euclideanRingInt);
  var fromEnum52 = /* @__PURE__ */ fromEnum(boundedEnumMinute);
  var fromEnum62 = /* @__PURE__ */ fromEnum(boundedEnumSecond);
  var fromEnum72 = /* @__PURE__ */ fromEnum(boundedEnumMillisecond);
  var div1 = /* @__PURE__ */ div(euclideanRingInt);
  var YearFull = /* @__PURE__ */ function() {
    function YearFull2() {
    }
    ;
    YearFull2.value = new YearFull2();
    return YearFull2;
  }();
  var YearTwoDigits = /* @__PURE__ */ function() {
    function YearTwoDigits2() {
    }
    ;
    YearTwoDigits2.value = new YearTwoDigits2();
    return YearTwoDigits2;
  }();
  var YearAbsolute = /* @__PURE__ */ function() {
    function YearAbsolute2() {
    }
    ;
    YearAbsolute2.value = new YearAbsolute2();
    return YearAbsolute2;
  }();
  var MonthFull = /* @__PURE__ */ function() {
    function MonthFull2() {
    }
    ;
    MonthFull2.value = new MonthFull2();
    return MonthFull2;
  }();
  var MonthShort = /* @__PURE__ */ function() {
    function MonthShort2() {
    }
    ;
    MonthShort2.value = new MonthShort2();
    return MonthShort2;
  }();
  var MonthTwoDigits = /* @__PURE__ */ function() {
    function MonthTwoDigits2() {
    }
    ;
    MonthTwoDigits2.value = new MonthTwoDigits2();
    return MonthTwoDigits2;
  }();
  var DayOfMonthTwoDigits = /* @__PURE__ */ function() {
    function DayOfMonthTwoDigits2() {
    }
    ;
    DayOfMonthTwoDigits2.value = new DayOfMonthTwoDigits2();
    return DayOfMonthTwoDigits2;
  }();
  var DayOfMonth = /* @__PURE__ */ function() {
    function DayOfMonth2() {
    }
    ;
    DayOfMonth2.value = new DayOfMonth2();
    return DayOfMonth2;
  }();
  var UnixTimestamp = /* @__PURE__ */ function() {
    function UnixTimestamp2() {
    }
    ;
    UnixTimestamp2.value = new UnixTimestamp2();
    return UnixTimestamp2;
  }();
  var DayOfWeek = /* @__PURE__ */ function() {
    function DayOfWeek2() {
    }
    ;
    DayOfWeek2.value = new DayOfWeek2();
    return DayOfWeek2;
  }();
  var DayOfWeekName = /* @__PURE__ */ function() {
    function DayOfWeekName2() {
    }
    ;
    DayOfWeekName2.value = new DayOfWeekName2();
    return DayOfWeekName2;
  }();
  var DayOfWeekNameShort = /* @__PURE__ */ function() {
    function DayOfWeekNameShort2() {
    }
    ;
    DayOfWeekNameShort2.value = new DayOfWeekNameShort2();
    return DayOfWeekNameShort2;
  }();
  var Hours24 = /* @__PURE__ */ function() {
    function Hours242() {
    }
    ;
    Hours242.value = new Hours242();
    return Hours242;
  }();
  var Hours12 = /* @__PURE__ */ function() {
    function Hours122() {
    }
    ;
    Hours122.value = new Hours122();
    return Hours122;
  }();
  var Meridiem = /* @__PURE__ */ function() {
    function Meridiem2() {
    }
    ;
    Meridiem2.value = new Meridiem2();
    return Meridiem2;
  }();
  var Minutes2 = /* @__PURE__ */ function() {
    function Minutes3() {
    }
    ;
    Minutes3.value = new Minutes3();
    return Minutes3;
  }();
  var MinutesTwoDigits = /* @__PURE__ */ function() {
    function MinutesTwoDigits2() {
    }
    ;
    MinutesTwoDigits2.value = new MinutesTwoDigits2();
    return MinutesTwoDigits2;
  }();
  var Seconds = /* @__PURE__ */ function() {
    function Seconds2() {
    }
    ;
    Seconds2.value = new Seconds2();
    return Seconds2;
  }();
  var SecondsTwoDigits = /* @__PURE__ */ function() {
    function SecondsTwoDigits2() {
    }
    ;
    SecondsTwoDigits2.value = new SecondsTwoDigits2();
    return SecondsTwoDigits2;
  }();
  var Milliseconds2 = /* @__PURE__ */ function() {
    function Milliseconds3() {
    }
    ;
    Milliseconds3.value = new Milliseconds3();
    return Milliseconds3;
  }();
  var MillisecondsShort = /* @__PURE__ */ function() {
    function MillisecondsShort2() {
    }
    ;
    MillisecondsShort2.value = new MillisecondsShort2();
    return MillisecondsShort2;
  }();
  var MillisecondsTwoDigits = /* @__PURE__ */ function() {
    function MillisecondsTwoDigits2() {
    }
    ;
    MillisecondsTwoDigits2.value = new MillisecondsTwoDigits2();
    return MillisecondsTwoDigits2;
  }();
  var Placeholder = /* @__PURE__ */ function() {
    function Placeholder2(value0) {
      this.value0 = value0;
    }
    ;
    Placeholder2.create = function(value0) {
      return new Placeholder2(value0);
    };
    return Placeholder2;
  }();
  var printShortMonth = function(v) {
    if (v instanceof January) {
      return "Jan";
    }
    ;
    if (v instanceof February) {
      return "Feb";
    }
    ;
    if (v instanceof March) {
      return "Mar";
    }
    ;
    if (v instanceof April) {
      return "Apr";
    }
    ;
    if (v instanceof May) {
      return "May";
    }
    ;
    if (v instanceof June) {
      return "Jun";
    }
    ;
    if (v instanceof July) {
      return "Jul";
    }
    ;
    if (v instanceof August) {
      return "Aug";
    }
    ;
    if (v instanceof September) {
      return "Sep";
    }
    ;
    if (v instanceof October) {
      return "Oct";
    }
    ;
    if (v instanceof November) {
      return "Nov";
    }
    ;
    if (v instanceof December) {
      return "Dec";
    }
    ;
    throw new Error("Failed pattern match at Data.Formatter.DateTime (line 489, column 19 - line 501, column 22): " + [v.constructor.name]);
  };
  var padSingleDigit = function(i2) {
    if (i2 < 0) {
      return "-" + padSingleDigit(-i2 | 0);
    }
    ;
    if (i2 < 10) {
      return "0" + show6(i2);
    }
    ;
    if (otherwise) {
      return show6(i2);
    }
    ;
    throw new Error("Failed pattern match at Data.Formatter.DateTime (line 194, column 1 - line 194, column 32): " + [i2.constructor.name]);
  };
  var padQuadrupleDigit = function(i2) {
    if (i2 < 0) {
      return "-" + padQuadrupleDigit(-i2 | 0);
    }
    ;
    if (i2 < 10) {
      return "000" + show6(i2);
    }
    ;
    if (i2 < 100) {
      return "00" + show6(i2);
    }
    ;
    if (i2 < 1e3) {
      return "0" + show6(i2);
    }
    ;
    if (otherwise) {
      return show6(i2);
    }
    ;
    throw new Error("Failed pattern match at Data.Formatter.DateTime (line 207, column 1 - line 207, column 35): " + [i2.constructor.name]);
  };
  var padDoubleDigit = function(i2) {
    if (i2 < 0) {
      return "-" + padDoubleDigit(-i2 | 0);
    }
    ;
    if (i2 < 10) {
      return "00" + show6(i2);
    }
    ;
    if (i2 < 100) {
      return "0" + show6(i2);
    }
    ;
    if (otherwise) {
      return show6(i2);
    }
    ;
    throw new Error("Failed pattern match at Data.Formatter.DateTime (line 200, column 1 - line 200, column 32): " + [i2.constructor.name]);
  };
  var formatYearTwoDigits = function(i2) {
    var dateString = show6(abs3(i2));
    var dateLength = length9(dateString);
    if (dateLength === 1) {
      return "0" + dateString;
    }
    ;
    if (dateLength === 2) {
      return dateString;
    }
    ;
    return drop4(dateLength - 2 | 0)(dateString);
  };
  var fix12 = function(h) {
    var $618 = h === 0;
    if ($618) {
      return 12;
    }
    ;
    return h;
  };
  var formatCommand = function(v) {
    return function(v1) {
      if (v1 instanceof YearFull) {
        return padQuadrupleDigit(fromEnum7(year(v.value0)));
      }
      ;
      if (v1 instanceof YearTwoDigits) {
        return formatYearTwoDigits(fromEnum7(year(v.value0)));
      }
      ;
      if (v1 instanceof YearAbsolute) {
        return show6(fromEnum7(year(v.value0)));
      }
      ;
      if (v1 instanceof MonthFull) {
        return show13(month(v.value0));
      }
      ;
      if (v1 instanceof MonthShort) {
        return printShortMonth(month(v.value0));
      }
      ;
      if (v1 instanceof MonthTwoDigits) {
        return padSingleDigit(fromEnum1(month(v.value0)));
      }
      ;
      if (v1 instanceof DayOfMonthTwoDigits) {
        return padSingleDigit(fromEnum22(day(v.value0)));
      }
      ;
      if (v1 instanceof DayOfMonth) {
        return show6(fromEnum22(day(v.value0)));
      }
      ;
      if (v1 instanceof UnixTimestamp) {
        return show6(floor2(function(v2) {
          return v2 / 1e3;
        }(unwrap6(unInstant(fromDateTime(v))))));
      }
      ;
      if (v1 instanceof DayOfWeek) {
        return show6(fromEnum32(weekday(v.value0)));
      }
      ;
      if (v1 instanceof DayOfWeekName) {
        return show24(weekday(v.value0));
      }
      ;
      if (v1 instanceof DayOfWeekNameShort) {
        return take4(3)(show24(weekday(v.value0)));
      }
      ;
      if (v1 instanceof Hours24) {
        return padSingleDigit(fromEnum42(hour(v.value1)));
      }
      ;
      if (v1 instanceof Hours12) {
        return padSingleDigit(fix12(mod4(fromEnum42(hour(v.value1)))(12)));
      }
      ;
      if (v1 instanceof Meridiem) {
        var $621 = fromEnum42(hour(v.value1)) >= 12;
        if ($621) {
          return "PM";
        }
        ;
        return "AM";
      }
      ;
      if (v1 instanceof Minutes2) {
        return show6(fromEnum52(minute(v.value1)));
      }
      ;
      if (v1 instanceof MinutesTwoDigits) {
        return padSingleDigit(fromEnum52(minute(v.value1)));
      }
      ;
      if (v1 instanceof Seconds) {
        return show6(fromEnum62(second(v.value1)));
      }
      ;
      if (v1 instanceof SecondsTwoDigits) {
        return padSingleDigit(fromEnum62(second(v.value1)));
      }
      ;
      if (v1 instanceof Milliseconds2) {
        return padDoubleDigit(fromEnum72(millisecond(v.value1)));
      }
      ;
      if (v1 instanceof MillisecondsShort) {
        return show6(function(v2) {
          return div1(v2)(100);
        }(fromEnum72(millisecond(v.value1))));
      }
      ;
      if (v1 instanceof MillisecondsTwoDigits) {
        return padSingleDigit(function(v2) {
          return div1(v2)(10);
        }(fromEnum72(millisecond(v.value1))));
      }
      ;
      if (v1 instanceof Placeholder) {
        return v1.value0;
      }
      ;
      throw new Error("Failed pattern match at Data.Formatter.DateTime (line 169, column 38 - line 192, column 21): " + [v1.constructor.name]);
    };
  };
  var format = function(f) {
    return function(d) {
      return foldMap12(formatCommand(d))(f);
    };
  };

  // output/Data.Tuple.Nested/index.js
  var tuple3 = function(a2) {
    return function(b2) {
      return function(c) {
        return new Tuple(a2, new Tuple(b2, new Tuple(c, unit)));
      };
    };
  };
  var get3 = function(v) {
    return v.value1.value1.value0;
  };
  var get22 = function(v) {
    return v.value1.value0;
  };
  var get1 = function(v) {
    return v.value0;
  };

  // output/Parsing.DataView/index.js
  var bind7 = /* @__PURE__ */ bind(bindParserT);
  var lift3 = /* @__PURE__ */ lift(monadTransParserT);
  var get4 = /* @__PURE__ */ get2();
  var Uint16IsSymbol = {
    reflectSymbol: function() {
      return "Uint16";
    }
  };
  var Uint8IsSymbol2 = {
    reflectSymbol: function() {
      return "Uint8";
    }
  };
  var Int8IsSymbol = {
    reflectSymbol: function() {
      return "Int8";
    }
  };
  var anyPrim = function() {
    return function(dictBytesPerType) {
      var get12 = get4(dictBytesPerType)();
      var byteWidth2 = byteWidth(dictBytesPerType);
      return function() {
        return function(dictIsSymbol) {
          var get23 = get12(dictIsSymbol);
          return function(dictMonadEffect) {
            var lift1 = lift3(dictMonadEffect.Monad0());
            var liftEffect10 = liftEffect(dictMonadEffect);
            return function(endian) {
              return function(v) {
                return bind7(getParserT)(function(v1) {
                  return bind7(lift1(liftEffect10(get23(endian)($$Proxy.value)(v1.value0)(v1.value1.index))))(function(v2) {
                    if (v2 instanceof Nothing) {
                      return fail2("anyPrim unexpected end of DataView");
                    }
                    ;
                    if (v2 instanceof Just) {
                      return stateParserT(function(v3) {
                        return new Tuple(v2.value0, new ParseState(v1.value0, {
                          line: 1,
                          column: 1,
                          index: v1.value1.index + byteWidth2($$Proxy.value) | 0
                        }, true));
                      });
                    }
                    ;
                    throw new Error("Failed pattern match at Parsing.DataView (line 111, column 72 - line 114, column 136): " + [v2.constructor.name]);
                  });
                });
              };
            };
          };
        };
      };
    };
  };
  var anyPrim1 = /* @__PURE__ */ anyPrim();
  var anyPrim2 = /* @__PURE__ */ anyPrim1(bytesPerTypeUint16)()(Uint16IsSymbol);
  var anyPrim4 = /* @__PURE__ */ anyPrim1(bytesPerTypeUint8)()(Uint8IsSymbol2);
  var anyPrim5 = /* @__PURE__ */ anyPrim1(bytesPerTypeInt8)()(Int8IsSymbol);
  var anyUint16be = function(dictMonadEffect) {
    return anyPrim2(dictMonadEffect)(BE.value)($$Proxy.value);
  };
  var anyUint8 = function(dictMonadEffect) {
    return anyPrim4(dictMonadEffect)(LE.value)($$Proxy.value);
  };
  var anyInt8 = function(dictMonadEffect) {
    return anyPrim5(dictMonadEffect)(LE.value)($$Proxy.value);
  };

  // output/EchonetLite/index.js
  var append7 = /* @__PURE__ */ append(semigroupArray);
  var map24 = /* @__PURE__ */ map(functorArray);
  var map112 = /* @__PURE__ */ map(functorMaybe);
  var unfoldr12 = /* @__PURE__ */ unfoldr1(unfoldable1Array);
  var discard6 = /* @__PURE__ */ discard(discardUnit)(bindMaybe);
  var guard2 = /* @__PURE__ */ guard(alternativeMaybe);
  var eq4 = /* @__PURE__ */ eq(uintEqInstance);
  var bind8 = /* @__PURE__ */ bind(bindMaybe);
  var apply3 = /* @__PURE__ */ apply(applyMaybe);
  var toEnum4 = /* @__PURE__ */ toEnum(boundedEnumYear);
  var toEnum1 = /* @__PURE__ */ toEnum(boundedEnumMonth);
  var toEnum23 = /* @__PURE__ */ toEnum(boundedEnumDay);
  var toEnum32 = /* @__PURE__ */ toEnum(boundedEnumHour);
  var toEnum42 = /* @__PURE__ */ toEnum(boundedEnumMinute);
  var toEnum5 = /* @__PURE__ */ toEnum(boundedEnumSecond);
  var toEnum6 = /* @__PURE__ */ toEnum(boundedEnumMillisecond);
  var bind15 = /* @__PURE__ */ bind(bindParserT);
  var replicateA2 = /* @__PURE__ */ replicateA(applicativeParserT)(unfoldableArray)(traversableArray);
  var pure16 = /* @__PURE__ */ pure(applicativeParserT);
  var map25 = /* @__PURE__ */ map(functorParserT);
  var genericShowConstructor2 = /* @__PURE__ */ genericShowConstructor(genericShowArgsNoArguments);
  var X80\u52D5\u4F5C\u72B6\u614B = /* @__PURE__ */ function() {
    function X80\u52D5\u4F5C\u72B6\u614B2(value0) {
      this.value0 = value0;
    }
    ;
    X80\u52D5\u4F5C\u72B6\u614B2.create = function(value0) {
      return new X80\u52D5\u4F5C\u72B6\u614B2(value0);
    };
    return X80\u52D5\u4F5C\u72B6\u614B2;
  }();
  var X81\u8A2D\u7F6E\u5834\u6240 = /* @__PURE__ */ function() {
    function X81\u8A2D\u7F6E\u5834\u62402(value0) {
      this.value0 = value0;
    }
    ;
    X81\u8A2D\u7F6E\u5834\u62402.create = function(value0) {
      return new X81\u8A2D\u7F6E\u5834\u62402(value0);
    };
    return X81\u8A2D\u7F6E\u5834\u62402;
  }();
  var X88\u7570\u5E38\u767A\u751F\u72B6\u614B = /* @__PURE__ */ function() {
    function X88\u7570\u5E38\u767A\u751F\u72B6\u614B2(value0) {
      this.value0 = value0;
    }
    ;
    X88\u7570\u5E38\u767A\u751F\u72B6\u614B2.create = function(value0) {
      return new X88\u7570\u5E38\u767A\u751F\u72B6\u614B2(value0);
    };
    return X88\u7570\u5E38\u767A\u751F\u72B6\u614B2;
  }();
  var X8a\u30E1\u30FC\u30AB\u30FC\u30B3\u30FC\u30C9 = /* @__PURE__ */ function() {
    function X8a\u30E1\u30FC\u30AB\u30FC\u30B3\u30FC\u30C92(value0) {
      this.value0 = value0;
    }
    ;
    X8a\u30E1\u30FC\u30AB\u30FC\u30B3\u30FC\u30C92.create = function(value0) {
      return new X8a\u30E1\u30FC\u30AB\u30FC\u30B3\u30FC\u30C92(value0);
    };
    return X8a\u30E1\u30FC\u30AB\u30FC\u30B3\u30FC\u30C92;
  }();
  var Xd3\u4FC2\u6570 = /* @__PURE__ */ function() {
    function Xd3\u4FC2\u65702(value0) {
      this.value0 = value0;
    }
    ;
    Xd3\u4FC2\u65702.create = function(value0) {
      return new Xd3\u4FC2\u65702(value0);
    };
    return Xd3\u4FC2\u65702;
  }();
  var Xd7\u7A4D\u7B97\u96FB\u529B\u91CF\u6709\u52B9\u6841\u6570 = /* @__PURE__ */ function() {
    function Xd7\u7A4D\u7B97\u96FB\u529B\u91CF\u6709\u52B9\u6841\u65702(value0) {
      this.value0 = value0;
    }
    ;
    Xd7\u7A4D\u7B97\u96FB\u529B\u91CF\u6709\u52B9\u6841\u65702.create = function(value0) {
      return new Xd7\u7A4D\u7B97\u96FB\u529B\u91CF\u6709\u52B9\u6841\u65702(value0);
    };
    return Xd7\u7A4D\u7B97\u96FB\u529B\u91CF\u6709\u52B9\u6841\u65702;
  }();
  var Xe0\u7A4D\u7B97\u96FB\u529B\u91CF\u8A08\u6E2C\u5024 = /* @__PURE__ */ function() {
    function Xe0\u7A4D\u7B97\u96FB\u529B\u91CF\u8A08\u6E2C\u50242(value0) {
      this.value0 = value0;
    }
    ;
    Xe0\u7A4D\u7B97\u96FB\u529B\u91CF\u8A08\u6E2C\u50242.create = function(value0) {
      return new Xe0\u7A4D\u7B97\u96FB\u529B\u91CF\u8A08\u6E2C\u50242(value0);
    };
    return Xe0\u7A4D\u7B97\u96FB\u529B\u91CF\u8A08\u6E2C\u50242;
  }();
  var Xe1\u7A4D\u7B97\u96FB\u529B\u91CF\u5358\u4F4D = /* @__PURE__ */ function() {
    function Xe1\u7A4D\u7B97\u96FB\u529B\u91CF\u5358\u4F4D2(value0) {
      this.value0 = value0;
    }
    ;
    Xe1\u7A4D\u7B97\u96FB\u529B\u91CF\u5358\u4F4D2.create = function(value0) {
      return new Xe1\u7A4D\u7B97\u96FB\u529B\u91CF\u5358\u4F4D2(value0);
    };
    return Xe1\u7A4D\u7B97\u96FB\u529B\u91CF\u5358\u4F4D2;
  }();
  var Xe2\u7A4D\u7B97\u96FB\u529B\u91CF\u8A08\u6E2C\u5024\u5C65\u6B741 = /* @__PURE__ */ function() {
    function Xe2\u7A4D\u7B97\u96FB\u529B\u91CF\u8A08\u6E2C\u5024\u5C65\u6B7412(value0) {
      this.value0 = value0;
    }
    ;
    Xe2\u7A4D\u7B97\u96FB\u529B\u91CF\u8A08\u6E2C\u5024\u5C65\u6B7412.create = function(value0) {
      return new Xe2\u7A4D\u7B97\u96FB\u529B\u91CF\u8A08\u6E2C\u5024\u5C65\u6B7412(value0);
    };
    return Xe2\u7A4D\u7B97\u96FB\u529B\u91CF\u8A08\u6E2C\u5024\u5C65\u6B7412;
  }();
  var Xe5\u7A4D\u7B97\u5C65\u6B74\u53CE\u96C6\u65E51 = /* @__PURE__ */ function() {
    function Xe5\u7A4D\u7B97\u5C65\u6B74\u53CE\u96C6\u65E512(value0) {
      this.value0 = value0;
    }
    ;
    Xe5\u7A4D\u7B97\u5C65\u6B74\u53CE\u96C6\u65E512.create = function(value0) {
      return new Xe5\u7A4D\u7B97\u5C65\u6B74\u53CE\u96C6\u65E512(value0);
    };
    return Xe5\u7A4D\u7B97\u5C65\u6B74\u53CE\u96C6\u65E512;
  }();
  var Xe7\u77AC\u6642\u96FB\u529B\u8A08\u6E2C\u5024 = /* @__PURE__ */ function() {
    function Xe7\u77AC\u6642\u96FB\u529B\u8A08\u6E2C\u50242(value0) {
      this.value0 = value0;
    }
    ;
    Xe7\u77AC\u6642\u96FB\u529B\u8A08\u6E2C\u50242.create = function(value0) {
      return new Xe7\u77AC\u6642\u96FB\u529B\u8A08\u6E2C\u50242(value0);
    };
    return Xe7\u77AC\u6642\u96FB\u529B\u8A08\u6E2C\u50242;
  }();
  var Xe8\u77AC\u6642\u96FB\u6D41\u8A08\u6E2C\u5024 = /* @__PURE__ */ function() {
    function Xe8\u77AC\u6642\u96FB\u6D41\u8A08\u6E2C\u50242(value0) {
      this.value0 = value0;
    }
    ;
    Xe8\u77AC\u6642\u96FB\u6D41\u8A08\u6E2C\u50242.create = function(value0) {
      return new Xe8\u77AC\u6642\u96FB\u6D41\u8A08\u6E2C\u50242(value0);
    };
    return Xe8\u77AC\u6642\u96FB\u6D41\u8A08\u6E2C\u50242;
  }();
  var Xea\u6B63\u65B9\u5411\u5B9A\u6642\u7A4D\u7B97\u96FB\u529B\u91CF\u8A08\u6E2C\u5024 = /* @__PURE__ */ function() {
    function Xea\u6B63\u65B9\u5411\u5B9A\u6642\u7A4D\u7B97\u96FB\u529B\u91CF\u8A08\u6E2C\u50242(value0) {
      this.value0 = value0;
    }
    ;
    Xea\u6B63\u65B9\u5411\u5B9A\u6642\u7A4D\u7B97\u96FB\u529B\u91CF\u8A08\u6E2C\u50242.create = function(value0) {
      return new Xea\u6B63\u65B9\u5411\u5B9A\u6642\u7A4D\u7B97\u96FB\u529B\u91CF\u8A08\u6E2C\u50242(value0);
    };
    return Xea\u6B63\u65B9\u5411\u5B9A\u6642\u7A4D\u7B97\u96FB\u529B\u91CF\u8A08\u6E2C\u50242;
  }();
  var Xeb\u9006\u65B9\u5411\u5B9A\u6642\u7A4D\u7B97\u96FB\u529B\u91CF\u8A08\u6E2C\u5024 = /* @__PURE__ */ function() {
    function Xeb\u9006\u65B9\u5411\u5B9A\u6642\u7A4D\u7B97\u96FB\u529B\u91CF\u8A08\u6E2C\u50242(value0) {
      this.value0 = value0;
    }
    ;
    Xeb\u9006\u65B9\u5411\u5B9A\u6642\u7A4D\u7B97\u96FB\u529B\u91CF\u8A08\u6E2C\u50242.create = function(value0) {
      return new Xeb\u9006\u65B9\u5411\u5B9A\u6642\u7A4D\u7B97\u96FB\u529B\u91CF\u8A08\u6E2C\u50242(value0);
    };
    return Xeb\u9006\u65B9\u5411\u5B9A\u6642\u7A4D\u7B97\u96FB\u529B\u91CF\u8A08\u6E2C\u50242;
  }();
  var Xed\u7A4D\u7B97\u5C65\u6B74\u53CE\u96C6\u65E52 = /* @__PURE__ */ function() {
    function Xed\u7A4D\u7B97\u5C65\u6B74\u53CE\u96C6\u65E522() {
    }
    ;
    Xed\u7A4D\u7B97\u5C65\u6B74\u53CE\u96C6\u65E522.value = new Xed\u7A4D\u7B97\u5C65\u6B74\u53CE\u96C6\u65E522();
    return Xed\u7A4D\u7B97\u5C65\u6B74\u53CE\u96C6\u65E522;
  }();
  var SetI = /* @__PURE__ */ function() {
    function SetI2() {
    }
    ;
    SetI2.value = new SetI2();
    return SetI2;
  }();
  var SetC = /* @__PURE__ */ function() {
    function SetC2() {
    }
    ;
    SetC2.value = new SetC2();
    return SetC2;
  }();
  var Get = /* @__PURE__ */ function() {
    function Get2() {
    }
    ;
    Get2.value = new Get2();
    return Get2;
  }();
  var INF_REQ = /* @__PURE__ */ function() {
    function INF_REQ2() {
    }
    ;
    INF_REQ2.value = new INF_REQ2();
    return INF_REQ2;
  }();
  var SetGet = /* @__PURE__ */ function() {
    function SetGet2() {
    }
    ;
    SetGet2.value = new SetGet2();
    return SetGet2;
  }();
  var Set_Res = /* @__PURE__ */ function() {
    function Set_Res2() {
    }
    ;
    Set_Res2.value = new Set_Res2();
    return Set_Res2;
  }();
  var Get_Res = /* @__PURE__ */ function() {
    function Get_Res2() {
    }
    ;
    Get_Res2.value = new Get_Res2();
    return Get_Res2;
  }();
  var INF = /* @__PURE__ */ function() {
    function INF2() {
    }
    ;
    INF2.value = new INF2();
    return INF2;
  }();
  var INFC = /* @__PURE__ */ function() {
    function INFC2() {
    }
    ;
    INFC2.value = new INFC2();
    return INFC2;
  }();
  var INFC_Res = /* @__PURE__ */ function() {
    function INFC_Res2() {
    }
    ;
    INFC_Res2.value = new INFC_Res2();
    return INFC_Res2;
  }();
  var SetGet_Res = /* @__PURE__ */ function() {
    function SetGet_Res2() {
    }
    ;
    SetGet_Res2.value = new SetGet_Res2();
    return SetGet_Res2;
  }();
  var SetI_SNA = /* @__PURE__ */ function() {
    function SetI_SNA2() {
    }
    ;
    SetI_SNA2.value = new SetI_SNA2();
    return SetI_SNA2;
  }();
  var SetC_SNA = /* @__PURE__ */ function() {
    function SetC_SNA2() {
    }
    ;
    SetC_SNA2.value = new SetC_SNA2();
    return SetC_SNA2;
  }();
  var Get_SNA = /* @__PURE__ */ function() {
    function Get_SNA2() {
    }
    ;
    Get_SNA2.value = new Get_SNA2();
    return Get_SNA2;
  }();
  var INF_SNA = /* @__PURE__ */ function() {
    function INF_SNA2() {
    }
    ;
    INF_SNA2.value = new INF_SNA2();
    return INF_SNA2;
  }();
  var SetGetSNA = /* @__PURE__ */ function() {
    function SetGetSNA2() {
    }
    ;
    SetGetSNA2.value = new SetGetSNA2();
    return SetGetSNA2;
  }();
  var toUIntESV = function($816) {
    return fromInt(function(v) {
      if (v instanceof SetI) {
        return 96;
      }
      ;
      if (v instanceof SetC) {
        return 97;
      }
      ;
      if (v instanceof Get) {
        return 98;
      }
      ;
      if (v instanceof INF_REQ) {
        return 99;
      }
      ;
      if (v instanceof SetGet) {
        return 110;
      }
      ;
      if (v instanceof Set_Res) {
        return 113;
      }
      ;
      if (v instanceof Get_Res) {
        return 114;
      }
      ;
      if (v instanceof INF) {
        return 115;
      }
      ;
      if (v instanceof INFC) {
        return 116;
      }
      ;
      if (v instanceof INFC_Res) {
        return 122;
      }
      ;
      if (v instanceof SetGet_Res) {
        return 126;
      }
      ;
      if (v instanceof SetI_SNA) {
        return 80;
      }
      ;
      if (v instanceof SetC_SNA) {
        return 81;
      }
      ;
      if (v instanceof Get_SNA) {
        return 82;
      }
      ;
      if (v instanceof INF_SNA) {
        return 83;
      }
      ;
      if (v instanceof SetGetSNA) {
        return 94;
      }
      ;
      throw new Error("Failed pattern match at EchonetLite (line 218, column 9 - line 237, column 26): " + [v.constructor.name]);
    }($816));
  };
  var toStringWhmProperty = /* @__PURE__ */ function() {
    var printDateTime = format(fromFoldable(foldableArray)([YearFull.value, new Placeholder("-"), MonthTwoDigits.value, new Placeholder("-"), DayOfMonthTwoDigits.value, new Placeholder("T"), Hours24.value, new Placeholder(":"), Minutes2.value, new Placeholder(":"), SecondsTwoDigits.value]));
    return function(v) {
      if (v instanceof X80\u52D5\u4F5C\u72B6\u614B) {
        return joinWith(" ")(["\u52D5\u4F5C\u72B6\u614B", maybe("")(function($817) {
          return function(v1) {
            if (v1) {
              return "ON";
            }
            ;
            return "OFF";
          }(function(v1) {
            return v1.on;
          }($817));
        })(v.value0)]);
      }
      ;
      if (v instanceof X81\u8A2D\u7F6E\u5834\u6240) {
        return joinWith(" ")(["\u8A2D\u7F6E\u5834\u6240", maybe("")(function() {
          var $818 = toStringAs(decimal);
          return function($819) {
            return $818(toInt($819));
          };
        }())(v.value0)]);
      }
      ;
      if (v instanceof X88\u7570\u5E38\u767A\u751F\u72B6\u614B) {
        return joinWith(" ")(["\u7570\u5E38\u767A\u751F\u72B6\u614B", maybe("")(function($820) {
          return function(v1) {
            if (v1) {
              return "\u7570\u5E38\u767A\u751F\u3042\u308A";
            }
            ;
            return "\u7570\u5E38\u767A\u751F\u306A\u3057";
          }(function(v1) {
            return v1.abnormal;
          }($820));
        })(v.value0)]);
      }
      ;
      if (v instanceof X8a\u30E1\u30FC\u30AB\u30FC\u30B3\u30FC\u30C9) {
        return joinWith(" ")(["\u30E1\u30FC\u30AB\u30FC\u30B3\u30FC\u30C9", maybe("")(function() {
          var $821 = toStringAs(hexadecimal);
          return function($822) {
            return $821(toInt(function(v1) {
              return v1.manufacturer;
            }($822)));
          };
        }())(v.value0)]);
      }
      ;
      if (v instanceof Xd3\u4FC2\u6570) {
        return joinWith(" ")(["\u4FC2\u6570", maybe("")(toStringAs(decimal))(v.value0)]);
      }
      ;
      if (v instanceof Xd7\u7A4D\u7B97\u96FB\u529B\u91CF\u6709\u52B9\u6841\u6570) {
        return joinWith(" ")(concat([["\u7A4D\u7B97\u96FB\u529B\u91CF\u6709\u52B9\u6841\u6570"], function() {
          if (v.value0 instanceof Nothing) {
            return [];
          }
          ;
          if (v.value0 instanceof Just) {
            return [toStringAs(decimal)(v.value0.value0), "digits"];
          }
          ;
          throw new Error("Failed pattern match at EchonetLite (line 467, column 13 - line 469, column 67): " + [v.value0.constructor.name]);
        }()]));
      }
      ;
      if (v instanceof Xe0\u7A4D\u7B97\u96FB\u529B\u91CF\u8A08\u6E2C\u5024) {
        return joinWith(" ")(["\u7A4D\u7B97\u96FB\u529B\u91CF\u8A08\u6E2C\u5024", maybe("")(toStringAs(decimal))(v.value0)]);
      }
      ;
      if (v instanceof Xe1\u7A4D\u7B97\u96FB\u529B\u91CF\u5358\u4F4D) {
        return joinWith(" ")(["\u7A4D\u7B97\u96FB\u529B\u91CF\u5358\u4F4D", maybe("")(function() {
          var $823 = toStringWith(fixed(4));
          return function($824) {
            return $823(function(v1) {
              return v1.kwh\u4E57\u7387n;
            }($824));
          };
        }())(v.value0)]);
      }
      ;
      if (v instanceof Xe2\u7A4D\u7B97\u96FB\u529B\u91CF\u8A08\u6E2C\u5024\u5C65\u6B741) {
        return joinWith(" ")(concat([["\u7A4D\u7B97\u96FB\u529B\u91CF\u8A08\u6E2C\u5024\u5C65\u6B741"], function() {
          if (v.value0 instanceof Nothing) {
            return [];
          }
          ;
          if (v.value0 instanceof Just) {
            return append7(["day:" + toStringAs(decimal)(v.value0.value0.day), "cwh:"])(map24(maybe("NA")(toStringAs(decimal)))(v.value0.value0.cumlativeWH));
          }
          ;
          throw new Error("Failed pattern match at EchonetLite (line 485, column 13 - line 489, column 81): " + [v.value0.constructor.name]);
        }()]));
      }
      ;
      if (v instanceof Xe5\u7A4D\u7B97\u5C65\u6B74\u53CE\u96C6\u65E51) {
        return joinWith(" ")(concat([["\u7A4D\u7B97\u5C65\u6B74\u53CE\u96C6\u65E51"], maybe([])(function(x) {
          return ["day:", toStringAs(decimal)(x)];
        })(v.value0)]));
      }
      ;
      if (v instanceof Xe7\u77AC\u6642\u96FB\u529B\u8A08\u6E2C\u5024) {
        return joinWith(" ")(concat([["\u77AC\u6642\u96FB\u529B\u8A08\u6E2C\u5024"], maybe([])(function(x) {
          return [toStringAs(decimal)(x.watt), "W"];
        })(v.value0)]));
      }
      ;
      if (v instanceof Xe8\u77AC\u6642\u96FB\u6D41\u8A08\u6E2C\u5024) {
        return joinWith(" ")(concat([["\u77AC\u6642\u96FB\u6D41\u8A08\u6E2C\u5024"], function() {
          if (v.value0 instanceof Nothing) {
            return [];
          }
          ;
          if (v.value0 instanceof Just) {
            return ["R:", toStringAs(decimal)(v.value0.value0.deciAmpereR), "deciA", "T:", toStringAs(decimal)(v.value0.value0.deciAmpereT), "deciA"];
          }
          ;
          throw new Error("Failed pattern match at EchonetLite (line 507, column 13 - line 516, column 18): " + [v.value0.constructor.name]);
        }()]));
      }
      ;
      if (v instanceof Xea\u6B63\u65B9\u5411\u5B9A\u6642\u7A4D\u7B97\u96FB\u529B\u91CF\u8A08\u6E2C\u5024) {
        return joinWith(" ")(concat([["\u6B63\u65B9\u5411\u5B9A\u6642\u7A4D\u7B97\u96FB\u529B\u91CF\u8A08\u6E2C\u5024"], function() {
          if (v.value0 instanceof Nothing) {
            return [];
          }
          ;
          if (v.value0 instanceof Just) {
            return [printDateTime(v.value0.value0.datetime), toStringAs(decimal)(v.value0.value0.cumlativeWH)];
          }
          ;
          throw new Error("Failed pattern match at EchonetLite (line 522, column 13 - line 527, column 18): " + [v.value0.constructor.name]);
        }()]));
      }
      ;
      if (v instanceof Xeb\u9006\u65B9\u5411\u5B9A\u6642\u7A4D\u7B97\u96FB\u529B\u91CF\u8A08\u6E2C\u5024) {
        return joinWith(" ")(concat([["\u9006\u65B9\u5411\u5B9A\u6642\u7A4D\u7B97\u96FB\u529B\u91CF\u8A08\u6E2C\u5024"], function() {
          if (v.value0 instanceof Nothing) {
            return [];
          }
          ;
          if (v.value0 instanceof Just) {
            return [printDateTime(v.value0.value0.datetime), toStringAs(decimal)(v.value0.value0.cumlativeWH)];
          }
          ;
          throw new Error("Failed pattern match at EchonetLite (line 533, column 13 - line 538, column 18): " + [v.value0.constructor.name]);
        }()]));
      }
      ;
      if (v instanceof Xed\u7A4D\u7B97\u5C65\u6B74\u53CE\u96C6\u65E52) {
        return "\u7A4D\u7B97\u5C65\u6B74\u53CE\u96C6\u65E52";
      }
      ;
      throw new Error("Failed pattern match at EchonetLite (line 437, column 23 - line 540, column 28): " + [v.constructor.name]);
    };
  }();
  var toBigEndiannessTID = function(v) {
    var lo = and2(v)(fromInt(255));
    var hi = and2(shr(v)(fromInt(8)))(fromInt(255));
    return [hi, lo];
  };
  var toBigEndiannessEOJ = function(v) {
    return [get1(v), get22(v), get3(v)];
  };
  var toArrayArray = function(n) {
    var toMaybe2 = function() {
      var $825 = map112(toArray);
      return function($826) {
        return $825(fromArray($826));
      };
    }();
    var chop = function(xs) {
      return new Tuple(take(n)(xs), toMaybe2(drop(n)(xs)));
    };
    return unfoldr12(chop);
  };
  var stringWithZeroPadding = function(length13) {
    return function(input3) {
      var paddings = replicate2(length13)(codePointFromChar("0"));
      var arr = append7(paddings)(toCodePointArray(input3));
      return fromCodePointArray(reverse2(take(length13)(reverse2(arr))));
    };
  };
  var toStringEOJ = function(eoj) {
    var toStr = function(x) {
      return stringWithZeroPadding(2)(toStringAs(hexadecimal)(toInt(x)));
    };
    return joinWith("")(map24(toStr)(toBigEndiannessEOJ(eoj)));
  };
  var toStringTID = function(a2) {
    var toStr = function(x) {
      return stringWithZeroPadding(2)(toStringAs(hexadecimal)(toInt(x)));
    };
    return joinWith("")(map24(toStr)(toBigEndiannessTID(a2)));
  };
  var makeSmartWhmProperty = function(v) {
    var fromUInt8 = function(xs) {
      return map112(toInt)(head3(xs));
    };
    var fromUInt32BE = function() {
      var ui8 = fromInt(8);
      var ui24 = fromInt(24);
      var ui16 = fromInt(16);
      return function(v12) {
        if (v12.length === 4) {
          return toInt$prime(or2(or2(or2(shl(v12[0])(ui24))(shl(v12[1])(ui16)))(shl(v12[2])(ui8)))(v12[3]));
        }
        ;
        return Nothing.value;
      };
    }();
    var fromUInt16BE = function() {
      var ui8 = fromInt(8);
      return function(v12) {
        if (v12.length === 2) {
          return toInt$prime(or2(shl(v12[0])(ui8))(v12[1]));
        }
        ;
        return Nothing.value;
      };
    }();
    var v1 = toInt(v.epc);
    if (v1 === 128) {
      return new Just(new X80\u52D5\u4F5C\u72B6\u614B(function() {
        if (v.edt.length === 1) {
          if (eq4(v["edt"][0])(fromInt(48))) {
            return new Just({
              on: true
            });
          }
          ;
          if (eq4(v["edt"][0])(fromInt(49))) {
            return new Just({
              on: false
            });
          }
          ;
        }
        ;
        return Nothing.value;
      }()));
    }
    ;
    if (v1 === 129) {
      return new Just(new X81\u8A2D\u7F6E\u5834\u6240(function() {
        if (v.edt.length === 1) {
          return new Just(v["edt"][0]);
        }
        ;
        return Nothing.value;
      }()));
    }
    ;
    if (v1 === 136) {
      return new Just(new X88\u7570\u5E38\u767A\u751F\u72B6\u614B(function() {
        if (v.edt.length === 1) {
          if (eq4(v["edt"][0])(fromInt(65))) {
            return new Just({
              abnormal: true
            });
          }
          ;
          if (eq4(v["edt"][0])(fromInt(66))) {
            return new Just({
              abnormal: false
            });
          }
          ;
        }
        ;
        return Nothing.value;
      }()));
    }
    ;
    if (v1 === 138) {
      return new Just(new X8a\u30E1\u30FC\u30AB\u30FC\u30B3\u30FC\u30C9(function() {
        if (v.edt.length === 3) {
          return new Just({
            manufacturer: or2(or2(shl(v["edt"][0])(fromInt(16)))(shl(v["edt"][1])(fromInt(8))))(v["edt"][2])
          });
        }
        ;
        return Nothing.value;
      }()));
    }
    ;
    if (v1 === 211) {
      return new Just(new Xd3\u4FC2\u6570(new Just(fromMaybe(1)(fromUInt32BE(v.edt)))));
    }
    ;
    if (v1 === 215) {
      return new Just(new Xd7\u7A4D\u7B97\u96FB\u529B\u91CF\u6709\u52B9\u6841\u6570(fromUInt8(v.edt)));
    }
    ;
    if (v1 === 224) {
      return new Just(new Xe0\u7A4D\u7B97\u96FB\u529B\u91CF\u8A08\u6E2C\u5024(fromUInt32BE(v.edt)));
    }
    ;
    if (v1 === 225) {
      return new Just(new Xe1\u7A4D\u7B97\u96FB\u529B\u91CF\u5358\u4F4D(function() {
        if (v.edt.length === 1) {
          if (eq4(v["edt"][0])(fromInt(0))) {
            return new Just({
              kwh\u4E57\u7387n: 1
            });
          }
          ;
          if (eq4(v["edt"][0])(fromInt(1))) {
            return new Just({
              kwh\u4E57\u7387n: 0.1
            });
          }
          ;
          if (eq4(v["edt"][0])(fromInt(2))) {
            return new Just({
              kwh\u4E57\u7387n: 0.01
            });
          }
          ;
          if (eq4(v["edt"][0])(fromInt(3))) {
            return new Just({
              kwh\u4E57\u7387n: 1e-3
            });
          }
          ;
          if (eq4(v["edt"][0])(fromInt(4))) {
            return new Just({
              kwh\u4E57\u7387n: 1e-4
            });
          }
          ;
          if (eq4(v["edt"][0])(fromInt(10))) {
            return new Just({
              kwh\u4E57\u7387n: 10
            });
          }
          ;
          if (eq4(v["edt"][0])(fromInt(11))) {
            return new Just({
              kwh\u4E57\u7387n: 100
            });
          }
          ;
          if (eq4(v["edt"][0])(fromInt(12))) {
            return new Just({
              kwh\u4E57\u7387n: 1e3
            });
          }
          ;
          if (eq4(v["edt"][0])(fromInt(13))) {
            return new Just({
              kwh\u4E57\u7387n: 1e4
            });
          }
          ;
        }
        ;
        return Nothing.value;
      }()));
    }
    ;
    if (v1 === 226) {
      return new Just(new Xe2\u7A4D\u7B97\u96FB\u529B\u91CF\u8A08\u6E2C\u5024\u5C65\u6B741(discard6(guard2(eq4(v.pdc)(fromInt(194))))(function() {
        var maybeDay = fromUInt16BE(take(2)(v.edt));
        var array32bits = toArrayArray(4)(drop(2)(v.edt));
        var arrayMaybes = map24(fromUInt32BE)(array32bits);
        return map112(function(v2) {
          return {
            day: v2,
            cumlativeWH: arrayMaybes
          };
        })(maybeDay);
      })));
    }
    ;
    if (v1 === 229) {
      return new Just(new Xe5\u7A4D\u7B97\u5C65\u6B74\u53CE\u96C6\u65E51(fromUInt8(v.edt)));
    }
    ;
    if (v1 === 231) {
      return new Just(new Xe7\u77AC\u6642\u96FB\u529B\u8A08\u6E2C\u5024(map112(function(v2) {
        return {
          watt: v2
        };
      })(fromUInt32BE(v.edt))));
    }
    ;
    if (v1 === 232) {
      return new Just(new Xe8\u77AC\u6642\u96FB\u6D41\u8A08\u6E2C\u5024(bind8(fromUInt16BE(take(2)(v.edt)))(function(r) {
        return bind8(fromUInt16BE(drop(2)(v.edt)))(function(t) {
          return new Just({
            deciAmpereR: r,
            deciAmpereT: t
          });
        });
      })));
    }
    ;
    if (v1 === 234) {
      return new Just(new Xea\u6B63\u65B9\u5411\u5B9A\u6642\u7A4D\u7B97\u96FB\u529B\u91CF\u8A08\u6E2C\u5024(function() {
        if (v.edt.length === 11) {
          return bind8(apply3(apply3(map112(canonicalDate)(toEnum4((toInt(v["edt"][0]) * 256 | 0) + toInt(v["edt"][1]) | 0)))(toEnum1(toInt(v["edt"][2]))))(toEnum23(toInt(v["edt"][3]))))(function(date2) {
            return bind8(apply3(apply3(apply3(map112(Time.create)(toEnum32(toInt(v["edt"][4]))))(toEnum42(toInt(v["edt"][5]))))(toEnum5(toInt(v["edt"][6]))))(toEnum6(0)))(function(time4) {
              return bind8(fromUInt32BE([v["edt"][7], v["edt"][8], v["edt"][9], v["edt"][10]]))(function(cwh) {
                return new Just({
                  datetime: new DateTime(date2, time4),
                  cumlativeWH: cwh
                });
              });
            });
          });
        }
        ;
        return Nothing.value;
      }()));
    }
    ;
    if (v1 === 235) {
      return new Just(new Xeb\u9006\u65B9\u5411\u5B9A\u6642\u7A4D\u7B97\u96FB\u529B\u91CF\u8A08\u6E2C\u5024(function() {
        if (v.edt.length === 11) {
          return bind8(apply3(apply3(map112(canonicalDate)(toEnum4((toInt(v["edt"][0]) * 256 | 0) + toInt(v["edt"][1]) | 0)))(toEnum1(toInt(v["edt"][2]))))(toEnum23(toInt(v["edt"][3]))))(function(date2) {
            return bind8(apply3(apply3(apply3(map112(Time.create)(toEnum32(toInt(v["edt"][4]))))(toEnum42(toInt(v["edt"][5]))))(toEnum5(toInt(v["edt"][6]))))(toEnum6(0)))(function(time4) {
              return bind8(fromUInt32BE([v["edt"][7], v["edt"][8], v["edt"][9], v["edt"][10]]))(function(cwh) {
                return new Just({
                  datetime: new DateTime(date2, time4),
                  cumlativeWH: cwh
                });
              });
            });
          });
        }
        ;
        return Nothing.value;
      }()));
    }
    ;
    if (v1 === 237) {
      return new Just(Xed\u7A4D\u7B97\u5C65\u6B74\u53CE\u96C6\u65E52.value);
    }
    ;
    return Nothing.value;
  };
  var makeESV = function(x) {
    var v = toInt(x);
    if (v === 96) {
      return new Just(SetI.value);
    }
    ;
    if (v === 97) {
      return new Just(SetC.value);
    }
    ;
    if (v === 98) {
      return new Just(Get.value);
    }
    ;
    if (v === 99) {
      return new Just(INF_REQ.value);
    }
    ;
    if (v === 110) {
      return new Just(SetGet.value);
    }
    ;
    if (v === 113) {
      return new Just(Set_Res.value);
    }
    ;
    if (v === 114) {
      return new Just(Get_Res.value);
    }
    ;
    if (v === 115) {
      return new Just(INF.value);
    }
    ;
    if (v === 116) {
      return new Just(INFC.value);
    }
    ;
    if (v === 122) {
      return new Just(INFC_Res.value);
    }
    ;
    if (v === 126) {
      return new Just(SetGet_Res.value);
    }
    ;
    if (v === 80) {
      return new Just(SetI_SNA.value);
    }
    ;
    if (v === 81) {
      return new Just(SetC_SNA.value);
    }
    ;
    if (v === 82) {
      return new Just(Get_SNA.value);
    }
    ;
    if (v === 83) {
      return new Just(INF_SNA.value);
    }
    ;
    if (v === 94) {
      return new Just(SetGetSNA.value);
    }
    ;
    return Nothing.value;
  };
  var parseEchonetLiteFrame = function(dictMonadRec) {
    var runParserT2 = runParserT(dictMonadRec);
    return function(dictMonadEffect) {
      var anyUint82 = anyUint8(dictMonadEffect);
      var anyUint16be2 = anyUint16be(dictMonadEffect);
      var anyInt82 = anyInt8(dictMonadEffect);
      return function(input3) {
        var prop3 = bind15(withErrorMessage(anyUint82)("EPC"))(function(epc) {
          return bind15(withErrorMessage(anyUint82)("PDC"))(function(pdc) {
            return bind15(withErrorMessage(replicateA2(toInt(pdc))(anyUint82))("EDT"))(function(edt) {
              return pure16({
                epc,
                pdc,
                edt
              });
            });
          });
        });
        return runParserT2(input3)(bind15(withErrorMessage(anyUint82)("EHD1"))(function(ehd1) {
          return bind15(withErrorMessage(anyUint82)("EHD2"))(function(ehd2) {
            return bind15(withErrorMessage(anyUint16be2)("TID"))(function(tid) {
              return bind15(withErrorMessage(replicateA2(3)(anyUint82))("SEOJ"))(function(seoj) {
                return bind15(withErrorMessage(replicateA2(3)(anyUint82))("DEOJ"))(function(deoj) {
                  return bind15(withErrorMessage(map25(makeESV)(anyUint82))("ESV"))(function(maybeESV) {
                    return bind15(withErrorMessage(anyInt82)("OPC"))(function(opc) {
                      return bind15(withErrorMessage(replicateA2(opc)(prop3))("PROPS"))(function(props) {
                        if (maybeESV instanceof Nothing) {
                          return fail2("ESV is invalid code");
                        }
                        ;
                        if (maybeESV instanceof Just) {
                          return pure16({
                            ehd1,
                            ehd2,
                            tid,
                            seoj,
                            deoj,
                            esv: maybeESV.value0,
                            opc,
                            props
                          });
                        }
                        ;
                        throw new Error("Failed pattern match at EchonetLite (line 571, column 5 - line 583, column 12): " + [maybeESV.constructor.name]);
                      });
                    });
                  });
                });
              });
            });
          });
        }));
      };
    };
  };
  var makeEOJ = function(a2) {
    return function(b2) {
      return function(c) {
        return tuple3(a2)(b2)(c);
      };
    };
  };
  var getPDC = function(datum) {
    return fromInt(length4(datum.edt));
  };
  var getOPC = function(frame) {
    return fromInt(length8(frame.edata));
  };
  var genericESV = {
    to: function(x) {
      if (x instanceof Inl) {
        return SetI.value;
      }
      ;
      if (x instanceof Inr && x.value0 instanceof Inl) {
        return SetC.value;
      }
      ;
      if (x instanceof Inr && (x.value0 instanceof Inr && x.value0.value0 instanceof Inl)) {
        return Get.value;
      }
      ;
      if (x instanceof Inr && (x.value0 instanceof Inr && (x.value0.value0 instanceof Inr && x.value0.value0.value0 instanceof Inl))) {
        return INF_REQ.value;
      }
      ;
      if (x instanceof Inr && (x.value0 instanceof Inr && (x.value0.value0 instanceof Inr && (x.value0.value0.value0 instanceof Inr && x.value0.value0.value0.value0 instanceof Inl)))) {
        return SetGet.value;
      }
      ;
      if (x instanceof Inr && (x.value0 instanceof Inr && (x.value0.value0 instanceof Inr && (x.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0 instanceof Inr && x.value0.value0.value0.value0.value0 instanceof Inl))))) {
        return Set_Res.value;
      }
      ;
      if (x instanceof Inr && (x.value0 instanceof Inr && (x.value0.value0 instanceof Inr && (x.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0 instanceof Inr && x.value0.value0.value0.value0.value0.value0 instanceof Inl)))))) {
        return Get_Res.value;
      }
      ;
      if (x instanceof Inr && (x.value0 instanceof Inr && (x.value0.value0 instanceof Inr && (x.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0 instanceof Inr && x.value0.value0.value0.value0.value0.value0.value0 instanceof Inl))))))) {
        return INF.value;
      }
      ;
      if (x instanceof Inr && (x.value0 instanceof Inr && (x.value0.value0 instanceof Inr && (x.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && x.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inl)))))))) {
        return INFC.value;
      }
      ;
      if (x instanceof Inr && (x.value0 instanceof Inr && (x.value0.value0 instanceof Inr && (x.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && x.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inl))))))))) {
        return INFC_Res.value;
      }
      ;
      if (x instanceof Inr && (x.value0 instanceof Inr && (x.value0.value0 instanceof Inr && (x.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && x.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inl)))))))))) {
        return SetGet_Res.value;
      }
      ;
      if (x instanceof Inr && (x.value0 instanceof Inr && (x.value0.value0 instanceof Inr && (x.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && x.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inl))))))))))) {
        return SetI_SNA.value;
      }
      ;
      if (x instanceof Inr && (x.value0 instanceof Inr && (x.value0.value0 instanceof Inr && (x.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && x.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inl)))))))))))) {
        return SetC_SNA.value;
      }
      ;
      if (x instanceof Inr && (x.value0 instanceof Inr && (x.value0.value0 instanceof Inr && (x.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && x.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inl))))))))))))) {
        return Get_SNA.value;
      }
      ;
      if (x instanceof Inr && (x.value0 instanceof Inr && (x.value0.value0 instanceof Inr && (x.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && x.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inl)))))))))))))) {
        return INF_SNA.value;
      }
      ;
      if (x instanceof Inr && (x.value0 instanceof Inr && (x.value0.value0 instanceof Inr && (x.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && (x.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr && x.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Inr)))))))))))))) {
        return SetGetSNA.value;
      }
      ;
      throw new Error("Failed pattern match at EchonetLite (line 185, column 1 - line 185, column 44): " + [x.constructor.name]);
    },
    from: function(x) {
      if (x instanceof SetI) {
        return new Inl(NoArguments.value);
      }
      ;
      if (x instanceof SetC) {
        return new Inr(new Inl(NoArguments.value));
      }
      ;
      if (x instanceof Get) {
        return new Inr(new Inr(new Inl(NoArguments.value)));
      }
      ;
      if (x instanceof INF_REQ) {
        return new Inr(new Inr(new Inr(new Inl(NoArguments.value))));
      }
      ;
      if (x instanceof SetGet) {
        return new Inr(new Inr(new Inr(new Inr(new Inl(NoArguments.value)))));
      }
      ;
      if (x instanceof Set_Res) {
        return new Inr(new Inr(new Inr(new Inr(new Inr(new Inl(NoArguments.value))))));
      }
      ;
      if (x instanceof Get_Res) {
        return new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inl(NoArguments.value)))))));
      }
      ;
      if (x instanceof INF) {
        return new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inl(NoArguments.value))))))));
      }
      ;
      if (x instanceof INFC) {
        return new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inl(NoArguments.value)))))))));
      }
      ;
      if (x instanceof INFC_Res) {
        return new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inl(NoArguments.value))))))))));
      }
      ;
      if (x instanceof SetGet_Res) {
        return new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inl(NoArguments.value)))))))))));
      }
      ;
      if (x instanceof SetI_SNA) {
        return new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inl(NoArguments.value))))))))))));
      }
      ;
      if (x instanceof SetC_SNA) {
        return new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inl(NoArguments.value)))))))))))));
      }
      ;
      if (x instanceof Get_SNA) {
        return new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inl(NoArguments.value))))))))))))));
      }
      ;
      if (x instanceof INF_SNA) {
        return new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inl(NoArguments.value)))))))))))))));
      }
      ;
      if (x instanceof SetGetSNA) {
        return new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(new Inr(NoArguments.value)))))))))))))));
      }
      ;
      throw new Error("Failed pattern match at EchonetLite (line 185, column 1 - line 185, column 44): " + [x.constructor.name]);
    }
  };
  var showESV = {
    show: /* @__PURE__ */ genericShow(genericESV)(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "SetI";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "SetC";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "Get";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "INF_REQ";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "SetGet";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "Set_Res";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "Get_Res";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "INF";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "INFC";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "INFC_Res";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "SetGet_Res";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "SetI_SNA";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "SetC_SNA";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "Get_SNA";
      }
    }))(/* @__PURE__ */ genericShowSum(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "INF_SNA";
      }
    }))(/* @__PURE__ */ genericShowConstructor2({
      reflectSymbol: function() {
        return "SetGetSNA";
      }
    })))))))))))))))))
  };

  // output/Control.Monad.Except/index.js
  var unwrap7 = /* @__PURE__ */ unwrap();
  var runExcept = function($3) {
    return unwrap7(runExceptT($3));
  };

  // output/Foreign.Index/foreign.js
  function unsafeReadPropImpl(f, s, key, value15) {
    return value15 == null ? f : s(value15[key]);
  }

  // output/Foreign.Index/index.js
  var unsafeReadProp = function(dictMonad) {
    var fail3 = fail(dictMonad);
    var pure21 = pure(applicativeExceptT(dictMonad));
    return function(k) {
      return function(value15) {
        return unsafeReadPropImpl(fail3(new TypeMismatch("object", typeOf(value15))), pure21, k, value15);
      };
    };
  };
  var readProp = function(dictMonad) {
    return unsafeReadProp(dictMonad);
  };

  // output/Web.Event.Event/foreign.js
  function _currentTarget(e) {
    return e.currentTarget;
  }

  // output/Web.Event.Event/index.js
  var currentTarget = function($5) {
    return toMaybe(_currentTarget($5));
  };

  // output/Web.UIEvent.MouseEvent.EventTypes/index.js
  var click2 = "click";

  // output/Halogen.HTML.Events/index.js
  var map26 = /* @__PURE__ */ map(functorMaybe);
  var composeKleisli2 = /* @__PURE__ */ composeKleisli(bindMaybe);
  var composeKleisliFlipped3 = /* @__PURE__ */ composeKleisliFlipped(/* @__PURE__ */ bindExceptT(monadIdentity));
  var readProp2 = /* @__PURE__ */ readProp(monadIdentity);
  var readString2 = /* @__PURE__ */ readString(monadIdentity);
  var mouseHandler = unsafeCoerce2;
  var handler$prime = function(et) {
    return function(f) {
      return handler(et)(function(ev) {
        return map26(Action.create)(f(ev));
      });
    };
  };
  var handler2 = function(et) {
    return function(f) {
      return handler(et)(function(ev) {
        return new Just(new Action(f(ev)));
      });
    };
  };
  var onClick = /* @__PURE__ */ function() {
    var $15 = handler2(click2);
    return function($16) {
      return $15(mouseHandler($16));
    };
  }();
  var addForeignPropHandler = function(key) {
    return function(prop3) {
      return function(reader) {
        return function(f) {
          var go2 = function(a2) {
            return composeKleisliFlipped3(reader)(readProp2(prop3))(unsafeToForeign(a2));
          };
          return handler$prime(key)(composeKleisli2(currentTarget)(function(e) {
            return either($$const(Nothing.value))(function($85) {
              return Just.create(f($85));
            })(runExcept(go2(e)));
          }));
        };
      };
    };
  };
  var onValueChange = /* @__PURE__ */ addForeignPropHandler(change)("value")(readString2);
  var onValueInput = /* @__PURE__ */ addForeignPropHandler(input)("value")(readString2);

  // output/Halogen.Themes.Bootstrap5/index.js
  var top0 = "top-0";
  var toastHeader = "toast-header";
  var toastContainer = "toast-container";
  var toastBody = "toast-body";
  var toast = "toast";
  var textWhite = "text-white";
  var textWarning = "text-warning";
  var textDark = "text-dark";
  var show7 = "show";
  var row = "row";
  var positionFixed = "position-fixed";
  var pb5 = "pb-5";
  var p5 = "p-5";
  var p3 = "p-3";
  var p2 = "p-2";
  var navbarExpandLg = "navbar-expand-lg";
  var navbarBrand = "navbar-brand";
  var navbar = "navbar";
  var meAuto = "me-auto";
  var me1 = "me-1";
  var mb1 = "mb-1";
  var m5 = "m-5";
  var m3 = "m-3";
  var m2 = "m-2";
  var m1 = "m-1";
  var fs5 = "fs-5";
  var formControl = "form-control";
  var fixedBottom = "fixed-bottom";
  var end0 = "end-0";
  var disabled10 = "disabled";
  var containerFluid = "container-fluid";
  var container = "container";
  var collapse = "collapse";
  var colSm5 = "col-sm-5";
  var colSm2 = "col-sm-2";
  var colFormLabel = "col-form-label";
  var col3 = "col-3";
  var col1 = "col-1";
  var col2 = "col";
  var btnSm = "btn-sm";
  var btnSecondary = "btn-secondary";
  var btnPrimary = "btn-primary";
  var btnOutlinePrimary = "btn-outline-primary";
  var btnOutlineDark = "btn-outline-dark";
  var btnClose = "btn-close";
  var btn = "btn";
  var border2 = "border";
  var bgSuccess = "bg-success";
  var bgPrimary = "bg-primary";
  var bgLight = "bg-light";
  var bgInfo = "bg-info";
  var bgDark = "bg-dark";
  var badge = "badge";
  var alertSuccess = "alert-success";
  var alertPrimary = "alert-primary";
  var alertLight = "alert-light";
  var alertInfo = "alert-info";
  var alertDanger = "alert-danger";
  var alert2 = "alert";
  var accordionItem = "accordion-item";
  var accordionHeader = "accordion-header";
  var accordionCollapse = "accordion-collapse";
  var accordionButton = "accordion-button";
  var accordionBody = "accordion-body";
  var accordion = "accordion";

  // output/Web.Clipboard/foreign.js
  function clipboard(navigator3) {
    return function() {
      return navigator3.clipboard;
    };
  }
  function writeText(text6) {
    return function(clipboard2) {
      return function() {
        return clipboard2.writeText(text6);
      };
    };
  }

  // output/EditForm/index.js
  var map27 = /* @__PURE__ */ map(functorArray);
  var pure17 = /* @__PURE__ */ pure(applicativeHalogenM);
  var bind9 = /* @__PURE__ */ bind(bindMaybe);
  var bind16 = /* @__PURE__ */ bind(bindHalogenM);
  var bindFlipped9 = /* @__PURE__ */ bindFlipped(bindEffect);
  var modify_3 = /* @__PURE__ */ modify_2(monadStateHalogenM);
  var mempty2 = /* @__PURE__ */ mempty(/* @__PURE__ */ monoidHalogenM(monoidUnit));
  var gets2 = /* @__PURE__ */ gets(monadStateHalogenM);
  var when4 = /* @__PURE__ */ when(applicativeHalogenM);
  var notEq2 = /* @__PURE__ */ notEq(/* @__PURE__ */ eqMaybe(eqIpV6Address));
  var bindFlipped12 = /* @__PURE__ */ bindFlipped(bindMaybe);
  var $$void10 = /* @__PURE__ */ $$void(functorHalogenM);
  var value13 = /* @__PURE__ */ value12(isPropString);
  var eq5 = /* @__PURE__ */ eq(/* @__PURE__ */ eqMaybe(eqInt));
  var type_19 = /* @__PURE__ */ type_17(isPropButtonType);
  var show8 = /* @__PURE__ */ show(showESV);
  var append13 = /* @__PURE__ */ append(semigroupArray);
  var SubmitToEchonetLiteMessage = /* @__PURE__ */ function() {
    function SubmitToEchonetLiteMessage2(value0) {
      this.value0 = value0;
    }
    ;
    SubmitToEchonetLiteMessage2.create = function(value0) {
      return new SubmitToEchonetLiteMessage2(value0);
    };
    return SubmitToEchonetLiteMessage2;
  }();
  var Initialize2 = /* @__PURE__ */ function() {
    function Initialize5() {
    }
    ;
    Initialize5.value = new Initialize5();
    return Initialize5;
  }();
  var Finalize2 = /* @__PURE__ */ function() {
    function Finalize5() {
    }
    ;
    Finalize5.value = new Finalize5();
    return Finalize5;
  }();
  var HandleInput = /* @__PURE__ */ function() {
    function HandleInput3(value0) {
      this.value0 = value0;
    }
    ;
    HandleInput3.create = function(value0) {
      return new HandleInput3(value0);
    };
    return HandleInput3;
  }();
  var OnValueChangeInput = /* @__PURE__ */ function() {
    function OnValueChangeInput2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    OnValueChangeInput2.create = function(value0) {
      return function(value1) {
        return new OnValueChangeInput2(value0, value1);
      };
    };
    return OnValueChangeInput2;
  }();
  var OnClickCopyToClipboard = /* @__PURE__ */ function() {
    function OnClickCopyToClipboard2(value0) {
      this.value0 = value0;
    }
    ;
    OnClickCopyToClipboard2.create = function(value0) {
      return new OnClickCopyToClipboard2(value0);
    };
    return OnClickCopyToClipboard2;
  }();
  var OnClickSubmit = /* @__PURE__ */ function() {
    function OnClickSubmit2(value0) {
      this.value0 = value0;
    }
    ;
    OnClickSubmit2.create = function(value0) {
      return new OnClickSubmit2(value0);
    };
    return OnClickSubmit2;
  }();
  var OnClickAddEDatum = /* @__PURE__ */ function() {
    function OnClickAddEDatum2() {
    }
    ;
    OnClickAddEDatum2.value = new OnClickAddEDatum2();
    return OnClickAddEDatum2;
  }();
  var OnClickRemoveEDatum = /* @__PURE__ */ function() {
    function OnClickRemoveEDatum2(value0) {
      this.value0 = value0;
    }
    ;
    OnClickRemoveEDatum2.create = function(value0) {
      return new OnClickRemoveEDatum2(value0);
    };
    return OnClickRemoveEDatum2;
  }();
  var serializeFrame = function(frame) {
    var serializeEData = function(datum) {
      return concat([[datum.epc, getPDC(datum)], datum.edt]);
    };
    return concat([[frame.ehd1, frame.ehd2], toBigEndiannessTID(frame.tid), toBigEndiannessEOJ(frame.seoj), toBigEndiannessEOJ(frame.deoj), [toUIntESV(frame.esv)], [getOPC(frame)], concatMap(serializeEData)(toArray(frame.edata))]);
  };
  var makeCommandLine = function(ipv6) {
    return function(frame) {
      var toChars = function(str) {
        return map27(function($93) {
          return fromInt(toCharCode2(char(singleton8($93))));
        })(toCodePointArray(str));
      };
      var fr = serializeFrame(frame);
      var partialCommand = joinWith(" ")(["SKSENDTO", "1", maybe("[IpV6address]")(toStringIpV6Address)(ipv6), "0E1A", "1", toUpper(stringWithZeroPadding(4)(toStringAs(hexadecimal)(length4(fr))))]);
      return {
        header: toChars(partialCommand),
        payload: fr
      };
    };
  };
  var initialEDatum = {
    epc: /* @__PURE__ */ fromInt(231),
    edt: []
  };
  var initialState = function(input3) {
    return {
      maybeClipboard: Nothing.value,
      maybeIpV6Address: input3.maybeIpV6Address,
      frame: {
        ehd1: fromInt(16),
        ehd2: fromInt(129),
        tid: fromInt(4660),
        seoj: makeEOJ(fromInt(5))(fromInt(255))(fromInt(1)),
        deoj: makeEOJ(fromInt(2))(fromInt(136))(fromInt(1)),
        esv: Get.value,
        edata: singleton7(initialEDatum)
      }
    };
  };
  var iconTrash = /* @__PURE__ */ i([/* @__PURE__ */ classes(["bi", "bi-trash"])])([]);
  var iconPlus = /* @__PURE__ */ i([/* @__PURE__ */ classes(["bi", "bi-plus"])])([]);
  var handleQuery = function(dictMonadAff) {
    return function(v) {
      return pure17(new Just(v.value0));
    };
  };
  var handleAction = function(dictMonadAff) {
    var liftEffect10 = liftEffect(monadEffectHalogenM(dictMonadAff.MonadEffect0()));
    var updateAtEData = function(index5) {
      return function(edata) {
        return function(epc) {
          return bind9(index4(edata)(index5))(function(item) {
            var newItem = function() {
              var $54 = {};
              for (var $55 in item) {
                if ({}.hasOwnProperty.call(item, $55)) {
                  $54[$55] = item[$55];
                }
                ;
              }
              ;
              $54.epc = epc;
              return $54;
            }();
            return updateAt3(index5)(newItem)(edata);
          });
        };
      };
    };
    var removeEData = function(index5) {
      return function(edata) {
        return bind9(deleteAt2(index5)(edata))(function(newArray2) {
          return fromArray(newArray2);
        });
      };
    };
    var addEData = function(edata) {
      return function(datum) {
        return snoc5(edata)(datum);
      };
    };
    return function(v) {
      if (v instanceof Initialize2) {
        return bind16(liftEffect10(bindFlipped9(navigator2)(windowImpl)))(function(navigator3) {
          return bind16(liftEffect10(clipboard(navigator3)))(function(clipboard2) {
            return modify_3(function(v1) {
              var $58 = {};
              for (var $59 in v1) {
                if ({}.hasOwnProperty.call(v1, $59)) {
                  $58[$59] = v1[$59];
                }
                ;
              }
              ;
              $58.maybeClipboard = new Just(clipboard2);
              return $58;
            });
          });
        });
      }
      ;
      if (v instanceof Finalize2) {
        return mempty2;
      }
      ;
      if (v instanceof HandleInput) {
        return bind16(gets2(function(v1) {
          return v1.maybeIpV6Address;
        }))(function(current) {
          return when4(notEq2(current)(v.value0.maybeIpV6Address))(modify_3(function(v1) {
            var $61 = {};
            for (var $62 in v1) {
              if ({}.hasOwnProperty.call(v1, $62)) {
                $61[$62] = v1[$62];
              }
              ;
            }
            ;
            $61.maybeIpV6Address = v.value0.maybeIpV6Address;
            return $61;
          }));
        });
      }
      ;
      if (v instanceof OnValueChangeInput) {
        var newEpc = bindFlipped12(fromInt$prime)(fromStringAs(hexadecimal)(v.value1));
        return bind16(gets2(function(v1) {
          return v1.frame;
        }))(function(frame) {
          return maybe(mempty2)(function(x) {
            return modify_3(function(v1) {
              var $68 = {};
              for (var $69 in v1) {
                if ({}.hasOwnProperty.call(v1, $69)) {
                  $68[$69] = v1[$69];
                }
                ;
              }
              ;
              $68.frame = function() {
                var $65 = {};
                for (var $66 in frame) {
                  if ({}.hasOwnProperty.call(frame, $66)) {
                    $65[$66] = frame[$66];
                  }
                  ;
                }
                ;
                $65.edata = x;
                return $65;
              }();
              return $68;
            });
          })(bindFlipped12(updateAtEData(v.value0)(frame.edata))(newEpc));
        });
      }
      ;
      if (v instanceof OnClickCopyToClipboard) {
        return bind16(gets2(function(v1) {
          return v1.maybeClipboard;
        }))(function(maybeClipboard) {
          if (maybeClipboard instanceof Nothing) {
            return mempty2;
          }
          ;
          if (maybeClipboard instanceof Just) {
            return $$void10(liftEffect10(writeText(v.value0)(maybeClipboard.value0)));
          }
          ;
          throw new Error("Failed pattern match at EditForm (line 319, column 5 - line 321, column 78): " + [maybeClipboard.constructor.name]);
        });
      }
      ;
      if (v instanceof OnClickSubmit) {
        return raise(new SubmitToEchonetLiteMessage(v.value0));
      }
      ;
      if (v instanceof OnClickAddEDatum) {
        return bind16(gets2(function(v1) {
          return v1.frame;
        }))(function(frame) {
          return modify_3(function(v1) {
            var $80 = {};
            for (var $81 in v1) {
              if ({}.hasOwnProperty.call(v1, $81)) {
                $80[$81] = v1[$81];
              }
              ;
            }
            ;
            $80.frame = function() {
              var $77 = {};
              for (var $78 in frame) {
                if ({}.hasOwnProperty.call(frame, $78)) {
                  $77[$78] = frame[$78];
                }
                ;
              }
              ;
              $77.edata = addEData(frame.edata)(initialEDatum);
              return $77;
            }();
            return $80;
          });
        });
      }
      ;
      if (v instanceof OnClickRemoveEDatum) {
        return bind16(gets2(function(v1) {
          return v1.frame;
        }))(function(frame) {
          return maybe(mempty2)(function(x) {
            return modify_3(function(v1) {
              var $86 = {};
              for (var $87 in v1) {
                if ({}.hasOwnProperty.call(v1, $87)) {
                  $86[$87] = v1[$87];
                }
                ;
              }
              ;
              $86.frame = function() {
                var $83 = {};
                for (var $84 in frame) {
                  if ({}.hasOwnProperty.call(frame, $84)) {
                    $83[$84] = frame[$84];
                  }
                  ;
                }
                ;
                $83.edata = x;
                return $83;
              }();
              return $86;
            });
          })(removeEData(v.value0)(frame.edata));
        });
      }
      ;
      throw new Error("Failed pattern match at EditForm (line 302, column 16 - line 328, column 101): " + [v.constructor.name]);
    };
  };
  var formEData = function(index5) {
    return function(datum) {
      var item = function(label5) {
        return function(value1) {
          return function(trailer) {
            return div2([classes([row, m1])])([label4([classes([colSm2])])([text5(label5)]), span3([classes([colSm5])])([text5(value1)]), span3([classes([colSm5])])([text5(trailer)])]);
          };
        };
      };
      var epc = function(v) {
        var option3 = function(v1) {
          return function(t) {
            return option([value13(v1), selected2(eq5(fromStringAs(hexadecimal)(v1))(new Just(toInt(datum.epc))))])([text5(t)]);
          };
        };
        return div2([classes([row, m1])])([label4([classes([colSm2, colFormLabel])])([text5(v.label)]), div2([classes([colSm5])])([select3([class_(formControl), onValueChange(OnValueChangeInput.create(index5))])([option3("80")("0x80:\u52D5\u4F5C\u72B6\u614B"), option3("81")("0x81:\u8A2D\u7F6E\u5834\u6240"), option3("88")("0x88:\u7570\u5E38\u767A\u751F\u72B6\u614B"), option3("8A")("0x8A:\u30E1\u30FC\u30AB\u30FC\u30B3\u30FC\u30C9"), option3("D3")("0xD3:\u4FC2\u6570"), option3("D7")("0xD7:\u7A4D\u7B97\u96FB\u529B\u91CF\u6709\u52B9\u6841\u6570"), option3("E0")("0xE0:\u7A4D\u7B97\u96FB\u529B\u91CF\u8A08\u6E2C\u5024"), option3("E1")("0xE1:\u7A4D\u7B97\u96FB\u529B\u91CF\u5358\u4F4D"), option3("E2")("0xE2:\u7A4D\u7B97\u96FB\u529B\u91CF\u8A08\u6E2C\u5024\u5C65\u6B741"), option3("E7")("0xE7:\u77AC\u6642\u96FB\u529B\u8A08\u6E2C\u5024"), option3("E8")("0xE8:\u77AC\u6642\u96FB\u6D41\u8A08\u6E2C\u5024"), option3("EA")("0xEA:\u6B63\u65B9\u5411\u5B9A\u6642\u7A4D\u7B97\u96FB\u529B\u91CF\u8A08\u6E2C\u5024"), option3("EB")("0xEB:\u9006\u65B9\u5411\u5B9A\u6642\u7A4D\u7B97\u96FB\u529B\u91CF\u8A08\u6E2C\u5024")])]), div2([classes([colSm5])])([text5(v.trailer)])]);
      };
      return div2([classes([p2, border2])])([button([type_19(ButtonButton.value), classes([btn, btnSm, btnOutlineDark]), onClick(function(v) {
        return new OnClickRemoveEDatum(index5);
      })])([iconTrash]), epc({
        label: "EPC",
        trailer: "\u8981\u6C42"
      }), item("PDC")(toStringAs(hexadecimal)(toInt(getPDC(datum))))("\u3053\u306E\u5F8C\u306B\u7D9A\u304FEDT\u306E\u30D0\u30A4\u30C8\u6570"), item("EDT")("")("Get\u8981\u6C42\u306BEDT\u306F\u3042\u308A\u307E\u305B\u3093")]);
    };
  };
  var formFrame = function(state3) {
    var toHexStringCapitalized = function() {
      var $94 = stringWithZeroPadding(2);
      var $95 = toStringAs(hexadecimal);
      return function($96) {
        return toUpper($94($95(toInt($96))));
      };
    }();
    var item = function(label5) {
      return function(value1) {
        return function(trailer) {
          return div2([classes([row, m1])])([label4([classes([colSm2])])([text5(label5)]), span3([classes([colSm5])])([text5(value1)]), span3([classes([colSm5])])([text5(trailer)])]);
        };
      };
    };
    return div2([classes([p2, alert2, alertInfo])])([div2([classes([p2])])([text5("ECHONET Lite \u30D5\u30EC\u30FC\u30E0")]), div2([classes([p2])])([item("EHD1")(toHexStringCapitalized(state3.frame.ehd1))("\u56FA\u5B9A\u5024"), item("EHD2")(toHexStringCapitalized(state3.frame.ehd2))("\u56FA\u5B9A\u5024"), item("TID")(toUpper(toStringTID(state3.frame.tid)))("\u3053\u306E\u30D5\u30A9\u30FC\u30E0\u3067\u306F\u7DE8\u96C6\u3067\u304D\u307E\u305B\u3093"), item("SEOJ")(toUpper(toStringEOJ(state3.frame.seoj)))("\u30B3\u30F3\u30C8\u30ED\u30FC\u30E9\u30FC"), item("DEOJ")(toUpper(toStringEOJ(state3.frame.deoj)))("\u30B9\u30DE\u30FC\u30C8\u30E1\u30FC\u30BF\u30FC"), item("ESV")(toHexStringCapitalized(toUIntESV(state3.frame.esv)))(show8(state3.frame.esv) + "\u8981\u6C42"), item("OPC")(toHexStringCapitalized(getOPC(state3.frame)))("EDATA\u306E\u6570"), div2([classes([p2])])([div2([classes([m1])])([text5("EDATA"), span3([classes([m1])])([button([type_19(ButtonButton.value), classes([btn, btnSm, btnOutlineDark]), onClick(function(v) {
      return OnClickAddEDatum.value;
    })])([iconPlus])]), div2([classes([m1])])(toArray(mapWithIndex3(formEData)(state3.frame.edata)))])])])]);
  };
  var render = function(state3) {
    var whitespace = fromInt(32);
    var commandline = makeCommandLine(state3.maybeIpV6Address)(state3.frame);
    var displayMessage = function() {
      var enc = function($$char2) {
        return "\\x" + toUpper(stringWithZeroPadding(2)(toStringAs(hexadecimal)(toCharCode2($$char2))));
      };
      var pl = joinWith("")(map27(enc)(mapMaybe(function($97) {
        return fromCharCode3(toInt($97));
      })(commandline.payload)));
      var h = joinWith("")(map27(function($98) {
        return singleton8(codePointFromChar($98));
      })(mapMaybe(function($99) {
        return fromCharCode3(toInt($99));
      })(commandline.header)));
      return joinWith(" ")([h, pl]);
    }();
    return div_([h2_([text5("\u9001\u4FE1\u3059\u308BECHONET Lite \u96FB\u6587\u3092\u7D44\u307F\u7ACB\u3066\u308B")]), div2([classes([alert2, alertLight])])([pre([classes([p2, bgDark, textWhite])])([text5(displayMessage)]), div2([class_(row)])([div2([class_(col2)])([]), button([classes([m1, col3, btn, btnSm, btnOutlinePrimary]), type_19(ButtonButton.value), onClick(function(v) {
      return new OnClickCopyToClipboard(displayMessage);
    })])([text5("copy to clipboard")]), button([classes([m1, col3, btn, btnSm, btnPrimary]), type_19(ButtonButton.value), onClick(function(v) {
      return new OnClickSubmit(append13(commandline.header)(append13([whitespace])(commandline.payload)));
    })])([text5("\u9001\u4FE1")])])]), p_([text5("\u9001\u4FE1\u96FB\u6587\u306F\u30D0\u30A4\u30CA\u30EA\u306A\u306E\u3067\u305D\u308C\u3092\u7D44\u307F\u7ACB\u3066\u308B\u30D5\u30A9\u30FC\u30E0\u3092\u7528\u610F\u3057\u307E\u3057\u305F")]), formFrame(state3)]);
  };
  var component = function(dictMonadAff) {
    return mkComponent({
      initialState,
      render,
      "eval": mkEval({
        handleAction: handleAction(dictMonadAff),
        handleQuery: handleQuery(dictMonadAff),
        receive: function($100) {
          return Just.create(HandleInput.create($100));
        },
        initialize: new Just(Initialize2.value),
        finalize: new Just(Finalize2.value)
      })
    });
  };

  // output/Effect.Now/foreign.js
  function now() {
    return Date.now();
  }

  // output/Web.File.Blob/foreign.js
  function blobImpl(args) {
    return function(mediaType) {
      return new Blob(args, { type: mediaType });
    };
  }

  // output/Web.File.Blob/index.js
  var fromString3 = function(str) {
    return function(ct) {
      return blobImpl([str])(ct);
    };
  };

  // output/Web.File.Url/foreign.js
  function createObjectURL(blob) {
    return function() {
      return URL.createObjectURL(blob);
    };
  }

  // output/WebSerialApi.SerialPort/foreign.js
  var getReaderImpl = (serialport) => {
    return serialport.readable.getReader();
  };
  var releaseLockReaderImpl = (reader) => {
    return reader.releaseLock();
  };
  var readImpl = (reader) => {
    return reader.read();
  };
  var getWriterImpl = (serialport) => {
    return serialport.writable.getWriter();
  };
  var releaseLockWriterImpl = (writer) => {
    return writer.releaseLock();
  };
  var writeImpl = (writer, chunk) => {
    return writer.write(chunk);
  };
  var requestPortImpl = () => {
    return navigator.serial.requestPort();
  };
  var openPortImpl = (serialport, options2) => {
    return serialport.open(options2);
  };
  var closePortImpl = (serialport) => {
    return serialport.close();
  };
  var forgetPortImpl = (serialport) => {
    return serialport.forget();
  };

  // output/Control.Promise/foreign.js
  function thenImpl(promise2) {
    return function(errCB) {
      return function(succCB) {
        return function() {
          promise2.then(succCB, errCB);
        };
      };
    };
  }

  // output/Control.Promise/index.js
  var voidRight2 = /* @__PURE__ */ voidRight(functorEffect);
  var mempty3 = /* @__PURE__ */ mempty(monoidCanceler);
  var identity11 = /* @__PURE__ */ identity(categoryFn);
  var alt7 = /* @__PURE__ */ alt(/* @__PURE__ */ altExceptT(semigroupNonEmptyList)(monadIdentity));
  var unsafeReadTagged2 = /* @__PURE__ */ unsafeReadTagged(monadIdentity);
  var map28 = /* @__PURE__ */ map(/* @__PURE__ */ functorExceptT(functorIdentity));
  var readString3 = /* @__PURE__ */ readString(monadIdentity);
  var bind10 = /* @__PURE__ */ bind(bindAff);
  var liftEffect7 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var toAff$prime = function(customCoerce) {
    return function(p4) {
      return makeAff(function(cb) {
        return voidRight2(mempty3)(thenImpl(p4)(function($14) {
          return cb(Left.create(customCoerce($14)))();
        })(function($15) {
          return cb(Right.create($15))();
        }));
      });
    };
  };
  var coerce4 = function(fn) {
    return either(function(v) {
      return error("Promise failed, couldn't extract JS Error or String");
    })(identity11)(runExcept(alt7(unsafeReadTagged2("Error")(fn))(map28(error)(readString3(fn)))));
  };
  var toAff = /* @__PURE__ */ toAff$prime(coerce4);
  var toAffE = function(f) {
    return bind10(liftEffect7(f))(toAff);
  };

  // output/WebSerialApi.SerialPort/index.js
  var bind11 = /* @__PURE__ */ bind(bindAff);
  var pure18 = /* @__PURE__ */ pure(applicativeAff);
  var StopBits1 = /* @__PURE__ */ function() {
    function StopBits12() {
    }
    ;
    StopBits12.value = new StopBits12();
    return StopBits12;
  }();
  var StopBits2 = /* @__PURE__ */ function() {
    function StopBits22() {
    }
    ;
    StopBits22.value = new StopBits22();
    return StopBits22;
  }();
  var Chunk = /* @__PURE__ */ function() {
    function Chunk2(value0) {
      this.value0 = value0;
    }
    ;
    Chunk2.create = function(value0) {
      return new Chunk2(value0);
    };
    return Chunk2;
  }();
  var Closed = /* @__PURE__ */ function() {
    function Closed2() {
    }
    ;
    Closed2.value = new Closed2();
    return Closed2;
  }();
  var ParityNone = /* @__PURE__ */ function() {
    function ParityNone2() {
    }
    ;
    ParityNone2.value = new ParityNone2();
    return ParityNone2;
  }();
  var ParityEven = /* @__PURE__ */ function() {
    function ParityEven2() {
    }
    ;
    ParityEven2.value = new ParityEven2();
    return ParityEven2;
  }();
  var ParityOdd = /* @__PURE__ */ function() {
    function ParityOdd2() {
    }
    ;
    ParityOdd2.value = new ParityOdd2();
    return ParityOdd2;
  }();
  var FlowControlNone = /* @__PURE__ */ function() {
    function FlowControlNone2() {
    }
    ;
    FlowControlNone2.value = new FlowControlNone2();
    return FlowControlNone2;
  }();
  var FlowControlHardware = /* @__PURE__ */ function() {
    function FlowControlHardware2() {
    }
    ;
    FlowControlHardware2.value = new FlowControlHardware2();
    return FlowControlHardware2;
  }();
  var DataBits7 = /* @__PURE__ */ function() {
    function DataBits72() {
    }
    ;
    DataBits72.value = new DataBits72();
    return DataBits72;
  }();
  var DataBits8 = /* @__PURE__ */ function() {
    function DataBits82() {
    }
    ;
    DataBits82.value = new DataBits82();
    return DataBits82;
  }();
  var write3 = function(writer) {
    return function(chunk) {
      return toAffE(function() {
        return writeImpl(writer, chunk);
      });
    };
  };
  var requestPort = /* @__PURE__ */ toAffE(requestPortImpl);
  var releaseLockWriter = /* @__PURE__ */ runEffectFn1(releaseLockWriterImpl);
  var releaseLockReader = /* @__PURE__ */ runEffectFn1(releaseLockReaderImpl);
  var read3 = function(reader) {
    return bind11(toAffE(function() {
      return readImpl(reader);
    }))(function(v) {
      return pure18(function() {
        if (v.done) {
          return Closed.value;
        }
        ;
        return new Chunk(v.value);
      }());
    });
  };
  var getWriter = /* @__PURE__ */ runEffectFn1(getWriterImpl);
  var getReader = /* @__PURE__ */ runEffectFn1(getReaderImpl);
  var fromOpenPortOption = function(option3) {
    return {
      baudRate: option3.baudRate,
      bufferSize: option3.bufferSize,
      dataBits: function() {
        if (option3.dataBits instanceof DataBits7) {
          return 7;
        }
        ;
        if (option3.dataBits instanceof DataBits8) {
          return 8;
        }
        ;
        throw new Error("Failed pattern match at WebSerialApi.SerialPort (line 130, column 7 - line 132, column 23): " + [option3.dataBits.constructor.name]);
      }(),
      flowControl: function() {
        if (option3.flowControl instanceof FlowControlNone) {
          return "none";
        }
        ;
        if (option3.flowControl instanceof FlowControlHardware) {
          return "hardware";
        }
        ;
        throw new Error("Failed pattern match at WebSerialApi.SerialPort (line 134, column 7 - line 136, column 42): " + [option3.flowControl.constructor.name]);
      }(),
      parity: function() {
        if (option3.parity instanceof ParityNone) {
          return "none";
        }
        ;
        if (option3.parity instanceof ParityEven) {
          return "even";
        }
        ;
        if (option3.parity instanceof ParityOdd) {
          return "Odd";
        }
        ;
        throw new Error("Failed pattern match at WebSerialApi.SerialPort (line 138, column 7 - line 141, column 27): " + [option3.parity.constructor.name]);
      }(),
      stopBits: function() {
        if (option3.stopBits instanceof StopBits1) {
          return 1;
        }
        ;
        if (option3.stopBits instanceof StopBits2) {
          return 2;
        }
        ;
        throw new Error("Failed pattern match at WebSerialApi.SerialPort (line 143, column 7 - line 145, column 23): " + [option3.stopBits.constructor.name]);
      }()
    };
  };
  var openPort = function(serialport) {
    return function(openportoption) {
      return toAffE(function() {
        return openPortImpl(serialport, fromOpenPortOption(openportoption));
      });
    };
  };
  var forgetPort = function(serialport) {
    return toAffE(function() {
      return forgetPortImpl(serialport);
    });
  };
  var defaultOpenPortOption = /* @__PURE__ */ function() {
    return {
      baudRate: 115200,
      bufferSize: 255,
      dataBits: DataBits8.value,
      flowControl: FlowControlNone.value,
      parity: ParityNone.value,
      stopBits: StopBits1.value
    };
  }();
  var closePort = function(serialport) {
    return toAffE(function() {
      return closePortImpl(serialport);
    });
  };

  // output/Terminal/index.js
  var $runtime_lazy11 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var liftEffect8 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var foldM5 = /* @__PURE__ */ foldM(foldableArray)(/* @__PURE__ */ monadWriterT(monoidBuilder)(monadEffect));
  var putUint82 = /* @__PURE__ */ putUint8(monadEffectEffect);
  var mempty4 = /* @__PURE__ */ mempty(monoidUnit);
  var whole4 = /* @__PURE__ */ whole2(typedArrayUint8);
  var findIndex4 = /* @__PURE__ */ findIndex3(typedArrayUint8);
  var eq6 = /* @__PURE__ */ eq(uintEqInstance);
  var bind17 = /* @__PURE__ */ bind(bindAff);
  var discard7 = /* @__PURE__ */ discard(discardUnit);
  var discard12 = /* @__PURE__ */ discard7(bindAff);
  var pure19 = /* @__PURE__ */ pure(applicativeAff);
  var tailRecM4 = /* @__PURE__ */ tailRecM(monadRecAff);
  var bind22 = /* @__PURE__ */ bind(bindHalogenM);
  var append8 = /* @__PURE__ */ append(semigroupArray);
  var type_20 = /* @__PURE__ */ type_17(isPropInputType);
  var value14 = /* @__PURE__ */ value12(isPropString);
  var type_1 = /* @__PURE__ */ type_17(isPropButtonType);
  var liftEffect12 = /* @__PURE__ */ liftEffect(monadEffectEffect);
  var gets3 = /* @__PURE__ */ gets(monadStateHalogenM);
  var mempty1 = /* @__PURE__ */ mempty(/* @__PURE__ */ monoidHalogenM(monoidUnit));
  var map29 = /* @__PURE__ */ map(functorMaybe);
  var discard23 = /* @__PURE__ */ discard7(bindHalogenM);
  var modify_4 = /* @__PURE__ */ modify_2(monadStateHalogenM);
  var logShow2 = /* @__PURE__ */ logShow(showString);
  var bindFlipped10 = /* @__PURE__ */ bindFlipped(bindEffect);
  var traverse_7 = /* @__PURE__ */ traverse_(applicativeEffect)(foldableMaybe);
  var get5 = /* @__PURE__ */ get(monadStateHalogenM);
  var show9 = /* @__PURE__ */ show(showError);
  var logShow1 = /* @__PURE__ */ logShow(showError);
  var when5 = /* @__PURE__ */ when(applicativeHalogenM);
  var discard42 = /* @__PURE__ */ discard7(/* @__PURE__ */ bindWriterT(semigroupBuilder)(bindEffect));
  var mempty22 = /* @__PURE__ */ mempty(/* @__PURE__ */ monoidWriterT(applicativeEffect)(monoidBuilder)(monoidUnit));
  var putArrayBuffer2 = /* @__PURE__ */ putArrayBuffer(monadEffect);
  var pure23 = /* @__PURE__ */ pure(applicativeHalogenM);
  var fromFoldable3 = /* @__PURE__ */ fromFoldable2(foldableMaybe);
  var map113 = /* @__PURE__ */ map(functorArray);
  var map210 = /* @__PURE__ */ map(functorHalogenM);
  var OpenSerialPort = /* @__PURE__ */ function() {
    function OpenSerialPort2(value0) {
      this.value0 = value0;
    }
    ;
    OpenSerialPort2.create = function(value0) {
      return new OpenSerialPort2(value0);
    };
    return OpenSerialPort2;
  }();
  var CloseSerialPort = /* @__PURE__ */ function() {
    function CloseSerialPort2(value0) {
      this.value0 = value0;
    }
    ;
    CloseSerialPort2.create = function(value0) {
      return new CloseSerialPort2(value0);
    };
    return CloseSerialPort2;
  }();
  var SetCommandLineString = /* @__PURE__ */ function() {
    function SetCommandLineString2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    SetCommandLineString2.create = function(value0) {
      return function(value1) {
        return new SetCommandLineString2(value0, value1);
      };
    };
    return SetCommandLineString2;
  }();
  var SendCommand = /* @__PURE__ */ function() {
    function SendCommand2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    SendCommand2.create = function(value0) {
      return function(value1) {
        return new SendCommand2(value0, value1);
      };
    };
    return SendCommand2;
  }();
  var IsOpenedSerialPort = /* @__PURE__ */ function() {
    function IsOpenedSerialPort2(value0) {
      this.value0 = value0;
    }
    ;
    IsOpenedSerialPort2.create = function(value0) {
      return new IsOpenedSerialPort2(value0);
    };
    return IsOpenedSerialPort2;
  }();
  var Message = /* @__PURE__ */ function() {
    function Message2(value0) {
      this.value0 = value0;
    }
    ;
    Message2.create = function(value0) {
      return new Message2(value0);
    };
    return Message2;
  }();
  var MessageError = /* @__PURE__ */ function() {
    function MessageError2(value0) {
      this.value0 = value0;
    }
    ;
    MessageError2.create = function(value0) {
      return new MessageError2(value0);
    };
    return MessageError2;
  }();
  var ArraivalResponce = /* @__PURE__ */ function() {
    function ArraivalResponce2(value0) {
      this.value0 = value0;
    }
    ;
    ArraivalResponce2.create = function(value0) {
      return new ArraivalResponce2(value0);
    };
    return ArraivalResponce2;
  }();
  var SerialportOpened = /* @__PURE__ */ function() {
    function SerialportOpened2() {
    }
    ;
    SerialportOpened2.value = new SerialportOpened2();
    return SerialportOpened2;
  }();
  var SerialportClosed = /* @__PURE__ */ function() {
    function SerialportClosed2() {
    }
    ;
    SerialportClosed2.value = new SerialportClosed2();
    return SerialportClosed2;
  }();
  var Initialize3 = /* @__PURE__ */ function() {
    function Initialize5() {
    }
    ;
    Initialize5.value = new Initialize5();
    return Initialize5;
  }();
  var Finalize3 = /* @__PURE__ */ function() {
    function Finalize5() {
    }
    ;
    Finalize5.value = new Finalize5();
    return Finalize5;
  }();
  var HandleSerialPortOpen = /* @__PURE__ */ function() {
    function HandleSerialPortOpen2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    HandleSerialPortOpen2.create = function(value0) {
      return function(value1) {
        return new HandleSerialPortOpen2(value0, value1);
      };
    };
    return HandleSerialPortOpen2;
  }();
  var HandleSerialPortClose = /* @__PURE__ */ function() {
    function HandleSerialPortClose2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    HandleSerialPortClose2.create = function(value0) {
      return function(value1) {
        return new HandleSerialPortClose2(value0, value1);
      };
    };
    return HandleSerialPortClose2;
  }();
  var OnValueInputFormInputArea = /* @__PURE__ */ function() {
    function OnValueInputFormInputArea2(value0) {
      this.value0 = value0;
    }
    ;
    OnValueInputFormInputArea2.create = function(value0) {
      return new OnValueInputFormInputArea2(value0);
    };
    return OnValueInputFormInputArea2;
  }();
  var OnClickFormSendButton = /* @__PURE__ */ function() {
    function OnClickFormSendButton2() {
    }
    ;
    OnClickFormSendButton2.value = new OnClickFormSendButton2();
    return OnClickFormSendButton2;
  }();
  var HandleInput2 = /* @__PURE__ */ function() {
    function HandleInput3(value0) {
      this.value0 = value0;
    }
    ;
    HandleInput3.create = function(value0) {
      return new HandleInput3(value0);
    };
    return HandleInput3;
  }();
  var HandleSerialPortArrivalData = /* @__PURE__ */ function() {
    function HandleSerialPortArrivalData2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    HandleSerialPortArrivalData2.create = function(value0) {
      return function(value1) {
        return new HandleSerialPortArrivalData2(value0, value1);
      };
    };
    return HandleSerialPortArrivalData2;
  }();
  var HandleIdleCallback = /* @__PURE__ */ function() {
    function HandleIdleCallback2() {
    }
    ;
    HandleIdleCallback2.value = new HandleIdleCallback2();
    return HandleIdleCallback2;
  }();
  var writeSerialPortText = function(serialport) {
    return function(text6) {
      var release = function(writer) {
        return liftEffect8(releaseLockWriter(writer));
      };
      var encode2 = function(str) {
        return function __do2() {
          var encoder = newImpl3();
          return encode(str)(encoder);
        };
      };
      return function __do2() {
        var bytearray = encode2(text6)();
        var writer = getWriter(serialport)();
        return launchAff_($$finally(release(writer))(write3(writer)(bytearray)))();
      };
    };
  };
  var writeSerialPortBinary = function(serialport) {
    return function(arr) {
      var release = function(writer) {
        return liftEffect8(releaseLockWriter(writer));
      };
      return function __do2() {
        var buf = execPut(foldM5(function(v) {
          return function(x) {
            return putUint82(x);
          };
        })(mempty4)(arr))();
        var binary = whole4(buf)();
        var writer = getWriter(serialport)();
        return launchAff_($$finally(release(writer))(write3(writer)(binary)))();
      };
    };
  };
  var splitSingleLine = function(buf) {
    var linefeed = fromInt(10);
    return function __do2() {
      var v = findIndex4(function(c) {
        return function(v2) {
          return eq6(c)(linefeed);
        };
      })(buf)();
      if (v instanceof Nothing) {
        return Nothing.value;
      }
      ;
      if (v instanceof Just) {
        var line = slice3(0)(v.value0 + 1 | 0)(buf)();
        var remains = slice3(v.value0 + 1 | 0)(byteLength3(buf))(buf)();
        return new Just({
          line,
          remains
        });
      }
      ;
      throw new Error("Failed pattern match at Terminal (line 296, column 9 - line 301, column 55): " + [v.constructor.name]);
    };
  };
  var runReadSerialPortThread = function(dictMonadEffect) {
    return function(dictMonadAff) {
      var liftAff2 = liftAff(monadAffHalogenM(dictMonadAff));
      var liftEffect22 = liftEffect(monadEffectHalogenM(dictMonadAff.MonadEffect0()));
      return function(serialport) {
        var go2 = function(reader) {
          return function(listener) {
            return function(sid) {
              var release = liftEffect8(releaseLockReader(reader));
              var loop2 = function(v) {
                return bind17(read3(reader))(function(result) {
                  return discard12(liftEffect8(notify(listener)(new HandleSerialPortArrivalData(sid, result))))(function() {
                    if (result instanceof Closed) {
                      return pure19(new Done(unit));
                    }
                    ;
                    if (result instanceof Chunk) {
                      return pure19(new Loop(unit));
                    }
                    ;
                    throw new Error("Failed pattern match at Terminal (line 383, column 7 - line 385, column 49): " + [result.constructor.name]);
                  });
                });
              };
              return liftAff2(forkAff($$finally(release)(tailRecM4(loop2)(unit))));
            };
          };
        };
        return bind22(liftEffect22(getReader(serialport)))(function(reader) {
          return bind22(liftEffect22(create3))(function(v) {
            return bind22(subscribe2(v.emitter))(function(sid) {
              return go2(reader)(v.listener)(sid);
            });
          });
        });
      };
    };
  };
  var render2 = function(state3) {
    var terminaltext = [pre([classes([col2]), style("max-height: 30vh")])([text5(function() {
      var $147 = state3.terminalText === "";
      if ($147) {
        return " ";
      }
      ;
      return state3.terminalText;
    }())])];
    var portStatus = function() {
      var v = isJust(state3.maybeSerialport);
      if (v) {
        return {
          classes: [bgSuccess],
          text: "PORT OPENED"
        };
      }
      ;
      if (!v) {
        return {
          classes: [bgDark, textWarning],
          text: "PORT CLOSED"
        };
      }
      ;
      throw new Error("Failed pattern match at Terminal (line 136, column 16 - line 138, column 77): " + [v.constructor.name]);
    }();
    var statusline = [div2([classes(append8([p2, border2, me1, badge])(portStatus.classes))])([text5(portStatus.text)]), a([classes(concat([[btn, btnSm, btnOutlineDark], function() {
      var $149 = state3.terminalText === "";
      if ($149) {
        return [disabled10];
      }
      ;
      return [];
    }()])), href4(state3.saveObjectUrl), target5("_blank"), download3("terminal.log")])([text5("\u4FDD\u5B58")])];
    var commandline = div2([classes([row])])([input2([classes([col2, me1, formControl]), type_20(InputText.value), placeholder3("input here..."), onValueInput(function(ev) {
      return new OnValueInputFormInputArea(ev);
    }), value14(state3.commandlineText)]), button([type_1(ButtonButton.value), classes([col1, btn, btnPrimary]), onClick($$const(OnClickFormSendButton.value))])([text5("\u9001\u4FE1")])]);
    return div2([])([div2([class_(mb1)])(statusline), div2([classes([containerFluid])])([div2([classes([row, mb1, bgDark, textWhite])])(terminaltext), commandline])]);
  };
  var openSerialPort = function(dictMonadEffect) {
    return function(dictMonadAff) {
      var liftEffect22 = liftEffect(monadEffectHalogenM(dictMonadAff.MonadEffect0()));
      var open = bind17(requestPort)(function(port2) {
        return discard12(openPort(port2)(defaultOpenPortOption))(function() {
          return pure19(port2);
        });
      });
      var callback = function(listener) {
        return function(sid) {
          return function(x) {
            return liftEffect12(notify(listener)(new HandleSerialPortOpen(sid, x)));
          };
        };
      };
      return bind22(liftEffect22(create3))(function(v) {
        return bind22(subscribe2(v.emitter))(function(sid) {
          return liftEffect22(runAff_(callback(v.listener)(sid))(open));
        });
      });
    };
  };
  var initialState2 = function(v) {
    return {
      maybeIdleCallbackId: Nothing.value,
      maybeSerialport: Nothing.value,
      maybeReadFiber: Nothing.value,
      txHistories: Nil.value,
      rxDataArraybuffer: Nothing.value,
      commandlineText: "",
      terminalText: "",
      saveObjectUrl: ""
    };
  };
  var handleIdleCallback = function(dictMonadEffect) {
    var monadEffectHalogenM2 = monadEffectHalogenM(dictMonadEffect);
    var liftEffect22 = liftEffect(monadEffectHalogenM2);
    var parseResponce2 = parseResponce(monadEffectHalogenM2);
    return bind22(gets3(function(v) {
      return v.rxDataArraybuffer;
    }))(function(rxDataArraybuffer) {
      if (rxDataArraybuffer instanceof Nothing) {
        return mempty1;
      }
      ;
      if (rxDataArraybuffer instanceof Just) {
        return bind22(gets3(function(v) {
          return v.txHistories;
        }))(function(txHistories) {
          var latestSendText = map29(function(v) {
            return v.sendText;
          })(head(txHistories));
          return bind22(liftEffect22(whole4(rxDataArraybuffer.value0)))(function(buf) {
            return bind22(parseResponce2(latestSendText)(buf))(function(resp) {
              return discard23(function() {
                if (resp instanceof Right) {
                  return bind22(liftEffect22(now))(function(now2) {
                    return raise(new ArraivalResponce({
                      at: now2,
                      responce: resp.value0.responce
                    }));
                  });
                }
                ;
                if (resp instanceof Left) {
                  return mempty1;
                }
                ;
                throw new Error("Failed pattern match at Terminal (line 269, column 7 - line 273, column 25): " + [resp.constructor.name]);
              }())(function() {
                return bind22(liftEffect22(splitSingleLine(buf)))(function(maybeLine) {
                  return discard23(function() {
                    if (maybeLine instanceof Nothing) {
                      return mempty1;
                    }
                    ;
                    if (maybeLine instanceof Just) {
                      return discard23(modify_4(function(v) {
                        var $158 = {};
                        for (var $159 in v) {
                          if ({}.hasOwnProperty.call(v, $159)) {
                            $158[$159] = v[$159];
                          }
                          ;
                        }
                        ;
                        $158.rxDataArraybuffer = new Just(buffer2(maybeLine.value0.remains));
                        return $158;
                      }))(function() {
                        return bind22(liftEffect22(newImpl2(utf8)))(function(decoder) {
                          return bind22(liftEffect22(decode(maybeLine.value0.line)(decoder)))(function(string2) {
                            return bind22(gets3(function(v) {
                              return v.terminalText;
                            }))(function(terminalText) {
                              return discard23(modify_4(function(v) {
                                var $161 = {};
                                for (var $162 in v) {
                                  if ({}.hasOwnProperty.call(v, $162)) {
                                    $161[$162] = v[$162];
                                  }
                                  ;
                                }
                                ;
                                $161.terminalText = terminalText + string2;
                                return $161;
                              }))(function() {
                                return liftEffect22(logShow2(string2));
                              });
                            });
                          });
                        });
                      });
                    }
                    ;
                    throw new Error("Failed pattern match at Terminal (line 276, column 7 - line 285, column 40): " + [maybeLine.constructor.name]);
                  }())(function() {
                    return bind22(gets3(function(v) {
                      return v.terminalText;
                    }))(function(terminalText) {
                      var blob = fromString3(terminalText)("text/plain");
                      return bind22(liftEffect22(createObjectURL(blob)))(function(saveObjectUrl) {
                        return modify_4(function(v) {
                          var $167 = {};
                          for (var $168 in v) {
                            if ({}.hasOwnProperty.call(v, $168)) {
                              $167[$168] = v[$168];
                            }
                            ;
                          }
                          ;
                          $167.saveObjectUrl = saveObjectUrl;
                          return $167;
                        });
                      });
                    });
                  });
                });
              });
            });
          });
        });
      }
      ;
      throw new Error("Failed pattern match at Terminal (line 260, column 3 - line 291, column 52): " + [rxDataArraybuffer.constructor.name]);
    });
  };
  var emitRequestIdleCallback = function(val) {
    var cancel = function(callbackId) {
      return function __do2() {
        var window2 = windowImpl();
        return cancelIdleCallback(callbackId)(window2)();
      };
    };
    var go2 = function(emitter) {
      return liftEffect12(function __do2() {
        var ref2 = $$new(Nothing.value)();
        var $lazy_loop = $runtime_lazy11("loop", "Terminal", function() {
          return function __do3() {
            emitter(val)();
            var window2 = windowImpl();
            var callbackId = requestIdleCallback({
              timeout: 300
            })($lazy_loop(418))(window2)();
            return write(new Just(callbackId))(ref2)();
          };
        });
        var loop2 = $lazy_loop(415);
        loop2();
        return bindFlipped10(traverse_7(cancel))(read(ref2));
      });
    };
    return makeEmitter(go2);
  };
  var handleAction2 = function(dictMonadAff) {
    var liftAff2 = liftAff(monadAffHalogenM(dictMonadAff));
    var MonadEffect0 = dictMonadAff.MonadEffect0();
    var liftEffect22 = liftEffect(monadEffectHalogenM(MonadEffect0));
    var runReadSerialPortThread1 = runReadSerialPortThread(MonadEffect0)(dictMonadAff);
    var handleIdleCallback1 = handleIdleCallback(MonadEffect0);
    return function(v) {
      if (v instanceof Initialize3) {
        return bind22(subscribe2(emitRequestIdleCallback(HandleIdleCallback.value)))(function(sid) {
          return modify_4(function(v1) {
            var $172 = {};
            for (var $173 in v1) {
              if ({}.hasOwnProperty.call(v1, $173)) {
                $172[$173] = v1[$173];
              }
              ;
            }
            ;
            $172.maybeIdleCallbackId = new Just(sid);
            return $172;
          });
        });
      }
      ;
      if (v instanceof Finalize3) {
        return bind22(get5)(function(v1) {
          return discard23(maybe(mempty1)(unsubscribe2)(v1.maybeIdleCallbackId))(function() {
            return discard23(maybe(mempty1)(function() {
              var $249 = killFiber(error("kill to read loop fiber"));
              return function($250) {
                return liftAff2($249($250));
              };
            }())(v1.maybeReadFiber))(function() {
              return maybe(mempty1)(function($251) {
                return liftAff2(forgetPort($251));
              })(v1.maybeSerialport);
            });
          });
        });
      }
      ;
      if (v instanceof HandleSerialPortOpen && v.value1 instanceof Left) {
        return discard23(liftEffect22(log("port open error:" + show9(v.value1.value0))))(function() {
          return bind22(liftEffect22(now))(function(now2) {
            return discard23(raise(new MessageError({
              at: now2,
              error: v.value1.value0
            })))(function() {
              return discard23(raise(SerialportClosed.value))(function() {
                return unsubscribe2(v.value0);
              });
            });
          });
        });
      }
      ;
      if (v instanceof HandleSerialPortOpen && v.value1 instanceof Right) {
        return discard23(modify_4(function(v1) {
          var $179 = {};
          for (var $180 in v1) {
            if ({}.hasOwnProperty.call(v1, $180)) {
              $179[$180] = v1[$180];
            }
            ;
          }
          ;
          $179.maybeSerialport = new Just(v.value1.value0);
          return $179;
        }))(function() {
          return bind22(liftEffect22(now))(function(now2) {
            return discard23(raise(new Message({
              at: now2,
              message: "\u30B7\u30EA\u30A2\u30EB\u30DD\u30FC\u30C8\u63A5\u7D9A\u6210\u529F"
            })))(function() {
              return discard23(raise(SerialportOpened.value))(function() {
                return bind22(runReadSerialPortThread1(v.value1.value0))(function(fiber) {
                  return discard23(modify_4(function(v1) {
                    var $182 = {};
                    for (var $183 in v1) {
                      if ({}.hasOwnProperty.call(v1, $183)) {
                        $182[$183] = v1[$183];
                      }
                      ;
                    }
                    ;
                    $182.maybeReadFiber = new Just(fiber);
                    return $182;
                  }))(function() {
                    return discard23(liftEffect22(log("start serial port reader thread")))(function() {
                      return unsubscribe2(v.value0);
                    });
                  });
                });
              });
            });
          });
        });
      }
      ;
      if (v instanceof HandleSerialPortClose && v.value1 instanceof Left) {
        return discard23(liftEffect22(logShow1(v.value1.value0)))(function() {
          return discard23(modify_4(function(v1) {
            var $188 = {};
            for (var $189 in v1) {
              if ({}.hasOwnProperty.call(v1, $189)) {
                $188[$189] = v1[$189];
              }
              ;
            }
            ;
            $188.maybeSerialport = Nothing.value;
            return $188;
          }))(function() {
            return bind22(liftEffect22(now))(function(now2) {
              return discard23(raise(new MessageError({
                at: now2,
                error: v.value1.value0
              })))(function() {
                return discard23(raise(SerialportClosed.value))(function() {
                  return unsubscribe2(v.value0);
                });
              });
            });
          });
        });
      }
      ;
      if (v instanceof HandleSerialPortClose && v.value1 instanceof Right) {
        return discard23(modify_4(function(v1) {
          var $194 = {};
          for (var $195 in v1) {
            if ({}.hasOwnProperty.call(v1, $195)) {
              $194[$195] = v1[$195];
            }
            ;
          }
          ;
          $194.maybeSerialport = Nothing.value;
          return $194;
        }))(function() {
          return bind22(liftEffect22(now))(function(now2) {
            return discard23(raise(new Message({
              at: now2,
              message: "\u30B7\u30EA\u30A2\u30EB\u30DD\u30FC\u30C8\u3092\u9589\u3058\u307E\u3057\u305F"
            })))(function() {
              return discard23(raise(SerialportClosed.value))(function() {
                return unsubscribe2(v.value0);
              });
            });
          });
        });
      }
      ;
      if (v instanceof OnValueInputFormInputArea) {
        return modify_4(function(v1) {
          var $200 = {};
          for (var $201 in v1) {
            if ({}.hasOwnProperty.call(v1, $201)) {
              $200[$201] = v1[$201];
            }
            ;
          }
          ;
          $200.commandlineText = v.value0;
          return $200;
        });
      }
      ;
      if (v instanceof OnClickFormSendButton) {
        return bind22(get5)(function(v1) {
          return discard23(liftEffect22(log("SEND: " + v1.commandlineText)))(function() {
            return bind22(liftEffect22(now))(function(now2) {
              if (v1.maybeSerialport instanceof Nothing) {
                return raise(new MessageError({
                  at: now2,
                  error: error("\u30B7\u30EA\u30A2\u30EB\u30DD\u30FC\u30C8\u304C\u9589\u3058\u3066\u3044\u307E\u3059\u3002")
                }));
              }
              ;
              if (v1.maybeSerialport instanceof Just) {
                return discard23(liftEffect22(writeSerialPortText(v1.maybeSerialport.value0)(v1.commandlineText + "\r\n")))(function() {
                  var newHistories = new Cons({
                    at: now2,
                    sendText: v1.commandlineText
                  }, v1.txHistories);
                  return modify_4(function(v2) {
                    var $206 = {};
                    for (var $207 in v2) {
                      if ({}.hasOwnProperty.call(v2, $207)) {
                        $206[$207] = v2[$207];
                      }
                      ;
                    }
                    ;
                    $206.commandlineText = "";
                    $206.txHistories = newHistories;
                    return $206;
                  });
                });
              }
              ;
              throw new Error("Failed pattern match at Terminal (line 228, column 5 - line 234, column 73): " + [v1.maybeSerialport.constructor.name]);
            });
          });
        });
      }
      ;
      if (v instanceof HandleInput2 && v.value0 instanceof Nothing) {
        return mempty1;
      }
      ;
      if (v instanceof HandleInput2 && v.value0 instanceof Just) {
        return bind22(gets3(function(v1) {
          return v1.commandlineText;
        }))(function(currentText) {
          return when5(currentText !== v.value0.value0)(modify_4(function(v1) {
            var $211 = {};
            for (var $212 in v1) {
              if ({}.hasOwnProperty.call(v1, $212)) {
                $211[$212] = v1[$212];
              }
              ;
            }
            ;
            $211.commandlineText = v.value0.value0;
            return $211;
          }));
        });
      }
      ;
      if (v instanceof HandleSerialPortArrivalData && v.value1 instanceof Closed) {
        return bind22(liftEffect22(now))(function(now2) {
          return discard23(raise(new MessageError({
            at: now2,
            error: error("\u30B7\u30EA\u30A2\u30EB\u30DD\u30FC\u30C8\u304C\u9589\u3058\u3066\u3044\u307E\u3059\u3002")
          })))(function() {
            return discard23(unsubscribe2(v.value0))(function() {
              return bind22(gets3(function(v1) {
                return v1.maybeReadFiber;
              }))(function(maybeReadFiber) {
                return discard23(maybe(mempty1)(function() {
                  var $252 = killFiber(error("kill to read loop fiber"));
                  return function($253) {
                    return liftAff2($252($253));
                  };
                }())(maybeReadFiber))(function() {
                  return modify_4(function(v1) {
                    var $216 = {};
                    for (var $217 in v1) {
                      if ({}.hasOwnProperty.call(v1, $217)) {
                        $216[$217] = v1[$217];
                      }
                      ;
                    }
                    ;
                    $216.maybeReadFiber = Nothing.value;
                    return $216;
                  });
                });
              });
            });
          });
        });
      }
      ;
      if (v instanceof HandleSerialPortArrivalData && v.value1 instanceof Chunk) {
        return bind22(gets3(function(v1) {
          return v1.rxDataArraybuffer;
        }))(function(rxDataArraybuffer) {
          return bind22(liftEffect22(execPut(discard42(maybe(mempty22)(putArrayBuffer2)(rxDataArraybuffer))(function() {
            return putArrayBuffer2(buffer2(v.value1.value0));
          }))))(function(newBuf) {
            return modify_4(function(v1) {
              var $221 = {};
              for (var $222 in v1) {
                if ({}.hasOwnProperty.call(v1, $222)) {
                  $221[$222] = v1[$222];
                }
                ;
              }
              ;
              $221.rxDataArraybuffer = new Just(newBuf);
              return $221;
            });
          });
        });
      }
      ;
      if (v instanceof HandleIdleCallback) {
        return handleIdleCallback1;
      }
      ;
      throw new Error("Failed pattern match at Terminal (line 186, column 16 - line 255, column 43): " + [v.constructor.name]);
    };
  };
  var closeSerialPort = function(dictMonadEffect) {
    return function(dictMonadAff) {
      var liftEffect22 = liftEffect(monadEffectHalogenM(dictMonadAff.MonadEffect0()));
      var liftAff2 = liftAff(monadAffHalogenM(dictMonadAff));
      var callback = function(listener) {
        return function(sid) {
          return function(x) {
            return liftEffect12(notify(listener)(new HandleSerialPortClose(sid, x)));
          };
        };
      };
      var go2 = function(serialport) {
        return bind22(liftEffect22(create3))(function(v) {
          return bind22(subscribe2(v.emitter))(function(sid) {
            return bind22(liftEffect22(runAff(callback(v.listener)(sid))(closePort(serialport))))(function(fiber) {
              return liftAff2(joinFiber(fiber));
            });
          });
        });
      };
      return bind22(get5)(function(v) {
        return discard23(maybe(mempty1)(function() {
          var $254 = killFiber(error("kill to read loop fiber"));
          return function($255) {
            return liftAff2($254($255));
          };
        }())(v.maybeReadFiber))(function() {
          return discard23(maybe(mempty1)(go2)(v.maybeSerialport))(function() {
            return modify_4(function(v1) {
              var $231 = {};
              for (var $232 in v1) {
                if ({}.hasOwnProperty.call(v1, $232)) {
                  $231[$232] = v1[$232];
                }
                ;
              }
              ;
              $231.maybeReadFiber = Nothing.value;
              $231.maybeSerialport = Nothing.value;
              return $231;
            });
          });
        });
      });
    };
  };
  var handleQuery2 = function(dictMonadAff) {
    var MonadEffect0 = dictMonadAff.MonadEffect0();
    var openSerialPort1 = openSerialPort(MonadEffect0)(dictMonadAff);
    var closeSerialPort1 = closeSerialPort(MonadEffect0)(dictMonadAff);
    var liftEffect22 = liftEffect(monadEffectHalogenM(MonadEffect0));
    return function(v) {
      if (v instanceof OpenSerialPort) {
        return discard23(openSerialPort1)(function() {
          return pure23(new Just(v.value0));
        });
      }
      ;
      if (v instanceof CloseSerialPort) {
        return discard23(closeSerialPort1)(function() {
          return pure23(new Just(v.value0));
        });
      }
      ;
      if (v instanceof SetCommandLineString) {
        return bind22(gets3(function(v1) {
          return v1.commandlineText;
        }))(function(currentText) {
          return discard23(when5(currentText !== v.value0)(modify_4(function(v1) {
            var $237 = {};
            for (var $238 in v1) {
              if ({}.hasOwnProperty.call(v1, $238)) {
                $237[$238] = v1[$238];
              }
              ;
            }
            ;
            $237.commandlineText = v.value0;
            return $237;
          })))(function() {
            return pure23(new Just(v.value1));
          });
        });
      }
      ;
      if (v instanceof SendCommand) {
        var chars = concatMap(function($256) {
          return fromFoldable3(fromCharCode3(toInt($256)));
        })(v.value0);
        var printableCodePoints = filter2(isPrint)(map113(codePointFromChar)(chars));
        var printableText = joinWith("")(map113(singleton8)(printableCodePoints));
        return discard23(liftEffect22(log("SENDCOMMAND: " + printableText)))(function() {
          return bind22(get5)(function(v1) {
            return bind22(liftEffect22(now))(function(now2) {
              return discard23(function() {
                if (v1.maybeSerialport instanceof Nothing) {
                  return raise(new MessageError({
                    at: now2,
                    error: error("\u30B7\u30EA\u30A2\u30EB\u30DD\u30FC\u30C8\u304C\u9589\u3058\u3066\u3044\u307E\u3059\u3002")
                  }));
                }
                ;
                if (v1.maybeSerialport instanceof Just) {
                  return liftEffect22(writeSerialPortBinary(v1.maybeSerialport.value0)(v.value0));
                }
                ;
                throw new Error("Failed pattern match at Terminal (line 330, column 5 - line 333, column 62): " + [v1.maybeSerialport.constructor.name]);
              }())(function() {
                return pure23(new Just(v.value1));
              });
            });
          });
        });
      }
      ;
      if (v instanceof IsOpenedSerialPort) {
        return bind22(map210(isJust)(gets3(function(v1) {
          return v1.maybeSerialport;
        })))(function(v1) {
          return pure23(new Just(v.value0(v1)));
        });
      }
      ;
      throw new Error("Failed pattern match at Terminal (line 306, column 15 - line 337, column 27): " + [v.constructor.name]);
    };
  };
  var component2 = function(dictMonadAff) {
    return mkComponent({
      initialState: initialState2,
      render: render2,
      "eval": mkEval({
        handleAction: handleAction2(dictMonadAff),
        handleQuery: handleQuery2(dictMonadAff),
        receive: defaultEval.receive,
        initialize: new Just(Initialize3.value),
        finalize: new Just(Finalize3.value)
      })
    });
  };

  // output/WebSerialApi/foreign.js
  var available = () => {
    return "serial" in navigator;
  };

  // output/Page/index.js
  var forever2 = /* @__PURE__ */ forever(monadRecAff);
  var discard8 = /* @__PURE__ */ discard(discardUnit);
  var discard13 = /* @__PURE__ */ discard8(bindAff);
  var liftEffect9 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var type_21 = /* @__PURE__ */ type_17(isPropButtonType);
  var unwrap8 = /* @__PURE__ */ unwrap();
  var diff2 = /* @__PURE__ */ diff(durationMinutes);
  var map30 = /* @__PURE__ */ map(functorArray);
  var bind18 = /* @__PURE__ */ bind(bindHalogenM);
  var gets4 = /* @__PURE__ */ gets(monadStateHalogenM);
  var modify_5 = /* @__PURE__ */ modify_2(monadStateHalogenM);
  var pure20 = /* @__PURE__ */ pure(applicativeHalogenM);
  var discard24 = /* @__PURE__ */ discard8(bindHalogenM);
  var show10 = /* @__PURE__ */ show(showEPANDESC);
  var foldM6 = /* @__PURE__ */ foldM(foldableArray)(/* @__PURE__ */ monadWriterT(monoidBuilder)(monadEffect));
  var putUint83 = /* @__PURE__ */ putUint8(monadEffectEffect);
  var mempty5 = /* @__PURE__ */ mempty(monoidUnit);
  var parseEchonetLiteFrame2 = /* @__PURE__ */ parseEchonetLiteFrame(monadRecHalogenM);
  var show14 = /* @__PURE__ */ show(showERXUDP);
  var map114 = /* @__PURE__ */ map(functorMaybe);
  var show25 = /* @__PURE__ */ show(showIpV6Address);
  var show33 = /* @__PURE__ */ show(showER);
  var append14 = /* @__PURE__ */ append(semigroupArray);
  var logShow3 = /* @__PURE__ */ logShow(showString);
  var eq7 = /* @__PURE__ */ eq(eqDateTime);
  var terminalIsSymbol = {
    reflectSymbol: function() {
      return "terminal";
    }
  };
  var request2 = /* @__PURE__ */ request()(terminalIsSymbol)(ordUnit);
  var bindFlipped11 = /* @__PURE__ */ bindFlipped(bindHalogenM);
  var mempty12 = /* @__PURE__ */ mempty(/* @__PURE__ */ monoidHalogenM(monoidUnit));
  var tell3 = /* @__PURE__ */ tell2()(terminalIsSymbol)(ordUnit);
  var bind19 = /* @__PURE__ */ bind(bindMaybe);
  var show42 = /* @__PURE__ */ show(showAddress64);
  var slot2 = /* @__PURE__ */ slot();
  var slot1 = /* @__PURE__ */ slot2({
    reflectSymbol: function() {
      return "editform";
    }
  })(ordUnit);
  var slot22 = /* @__PURE__ */ slot2(terminalIsSymbol)(ordUnit);
  var ToastMessage = /* @__PURE__ */ function() {
    function ToastMessage2(value0) {
      this.value0 = value0;
    }
    ;
    ToastMessage2.create = function(value0) {
      return new ToastMessage2(value0);
    };
    return ToastMessage2;
  }();
  var ToastMessageError = /* @__PURE__ */ function() {
    function ToastMessageError2(value0) {
      this.value0 = value0;
    }
    ;
    ToastMessageError2.create = function(value0) {
      return new ToastMessageError2(value0);
    };
    return ToastMessageError2;
  }();
  var ToastMessageEvent = /* @__PURE__ */ function() {
    function ToastMessageEvent2(value0) {
      this.value0 = value0;
    }
    ;
    ToastMessageEvent2.create = function(value0) {
      return new ToastMessageEvent2(value0);
    };
    return ToastMessageEvent2;
  }();
  var ToastMessageLocalEcho = /* @__PURE__ */ function() {
    function ToastMessageLocalEcho2(value0) {
      this.value0 = value0;
    }
    ;
    ToastMessageLocalEcho2.create = function(value0) {
      return new ToastMessageLocalEcho2(value0);
    };
    return ToastMessageLocalEcho2;
  }();
  var Initialize4 = /* @__PURE__ */ function() {
    function Initialize5() {
    }
    ;
    Initialize5.value = new Initialize5();
    return Initialize5;
  }();
  var Finalize4 = /* @__PURE__ */ function() {
    function Finalize5() {
    }
    ;
    Finalize5.value = new Finalize5();
    return Finalize5;
  }();
  var Tick = /* @__PURE__ */ function() {
    function Tick2() {
    }
    ;
    Tick2.value = new Tick2();
    return Tick2;
  }();
  var OnClickOpenPortButton = /* @__PURE__ */ function() {
    function OnClickOpenPortButton2() {
    }
    ;
    OnClickOpenPortButton2.value = new OnClickOpenPortButton2();
    return OnClickOpenPortButton2;
  }();
  var OnClickClosePortButton = /* @__PURE__ */ function() {
    function OnClickClosePortButton2() {
    }
    ;
    OnClickClosePortButton2.value = new OnClickClosePortButton2();
    return OnClickClosePortButton2;
  }();
  var OnClickSetCommandLineButton = /* @__PURE__ */ function() {
    function OnClickSetCommandLineButton2(value0) {
      this.value0 = value0;
    }
    ;
    OnClickSetCommandLineButton2.create = function(value0) {
      return new OnClickSetCommandLineButton2(value0);
    };
    return OnClickSetCommandLineButton2;
  }();
  var OnClickToastHideButton = /* @__PURE__ */ function() {
    function OnClickToastHideButton2(value0) {
      this.value0 = value0;
    }
    ;
    OnClickToastHideButton2.create = function(value0) {
      return new OnClickToastHideButton2(value0);
    };
    return OnClickToastHideButton2;
  }();
  var HandleTerminalUpdate = /* @__PURE__ */ function() {
    function HandleTerminalUpdate2(value0) {
      this.value0 = value0;
    }
    ;
    HandleTerminalUpdate2.create = function(value0) {
      return new HandleTerminalUpdate2(value0);
    };
    return HandleTerminalUpdate2;
  }();
  var HandleEditFormUpdate = /* @__PURE__ */ function() {
    function HandleEditFormUpdate2(value0) {
      this.value0 = value0;
    }
    ;
    HandleEditFormUpdate2.create = function(value0) {
      return new HandleEditFormUpdate2(value0);
    };
    return HandleEditFormUpdate2;
  }();
  var timer = function(dictMonadAff) {
    var MonadEffect0 = dictMonadAff.MonadEffect0();
    var Monad0 = MonadEffect0.Monad0();
    var bind23 = bind(Monad0.Bind1());
    var liftEffect13 = liftEffect(MonadEffect0);
    var liftAff2 = liftAff(dictMonadAff);
    var pure110 = pure(Monad0.Applicative0());
    return function(val) {
      return bind23(liftEffect13(create3))(function(v) {
        return bind23(liftAff2(forkAff(forever2(discard13(delay(60))(function() {
          return liftEffect9(notify(v.listener)(val));
        })))))(function() {
          return pure110(v.emitter);
        });
      });
    };
  };
  var navigationBar = /* @__PURE__ */ nav([/* @__PURE__ */ classes([navbar, navbarExpandLg, bgPrimary, textWhite])])([/* @__PURE__ */ div2([/* @__PURE__ */ class_(containerFluid)])([/* @__PURE__ */ div2([/* @__PURE__ */ classes([navbarBrand])])([/* @__PURE__ */ text5("\u30B9\u30DE\u30FC\u30C8\u30E1\u30FC\u30BF\u30FC \u30EB\u30FC\u30C8 B \u30C6\u30B9\u30C8\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3")])])]);
  var initialState3 = function(v) {
    return {
      maybeTimer: Nothing.value,
      instant: unsafePerformEffect(now),
      availableWebSerialApi: false,
      serialportOpened: false,
      toastMessages: [],
      maybeEpandesc: Nothing.value,
      maybeIpV6Address: Nothing.value
    };
  };
  var iconExclamationTriangleFill = /* @__PURE__ */ i([/* @__PURE__ */ classes(["bi", "bi-exclamation-triangle-fill"])])([]);
  var iconExclamationCircleFill = /* @__PURE__ */ i([/* @__PURE__ */ classes(["bi", "bi-exclamation-circle-fill"])])([]);
  var iconCheckCircleFill = /* @__PURE__ */ i([/* @__PURE__ */ classes(["bi", "bi-check-circle-fill"])])([]);
  var successAleatDialog = function(s) {
    return div2([classes([m2, alert2, alertSuccess, fs5])])([iconCheckCircleFill, text5(s)]);
  };
  var toastDialog = function(state3) {
    var go2 = function(v) {
      var closeButton = button([type_21(ButtonButton.value), classes([btnClose, bgLight]), attr2("aria-label")("Close"), onClick(function(v1) {
        return new OnClickToastHideButton(v.value1);
      })])([]);
      var toast2 = function(attachclasses) {
        return function(header2) {
          return function(body2) {
            var diff1 = floor2(unwrap8(diff2(state3.instant)(body2.at)));
            var min5 = toStringAs(decimal)(diff1);
            return div2([classes([toast, function() {
              if (v.value0) {
                return "show";
              }
              ;
              return "hide";
            }()]), attr2("role")("alert"), attr2("aria-live")("assertive"), attr2("aria-atomic")("true")])([div2([classes(concat([[toastHeader], attachclasses]))])([strong([class_(meAuto)])(header2), small_(function() {
              var $145 = diff1 < 1;
              if ($145) {
                return [text5("just now")];
              }
              ;
              return [text5(min5 + " mins ago")];
            }()), closeButton]), div2([class_(toastBody)])([text5(body2.message)])]);
          };
        };
      };
      var toastMessage = toast2([textWhite, bgDark])([iconCheckCircleFill, text5("\u30E1\u30C3\u30BB\u30FC\u30B8")]);
      var toastMessageError = toast2([textWarning, bgDark])([iconExclamationCircleFill, text5("\u5931\u6557\u3057\u307E\u3057\u305F")]);
      var toastMessageEvent = toast2([textDark, bgInfo])([iconCheckCircleFill, text5("\u30A4\u30D9\u30F3\u30C8")]);
      var toastMessageLocalEcho = toast2([textDark, bgInfo])([iconCheckCircleFill, text5("\u30A8\u30B3\u30FC\u30D0\u30C3\u30AF")]);
      if (v.value1 instanceof ToastMessage) {
        return toastMessage(v.value1.value0);
      }
      ;
      if (v.value1 instanceof ToastMessageError) {
        return toastMessageError(v.value1.value0);
      }
      ;
      if (v.value1 instanceof ToastMessageEvent) {
        return toastMessageEvent(v.value1.value0);
      }
      ;
      if (v.value1 instanceof ToastMessageLocalEcho) {
        return toastMessageLocalEcho(v.value1.value0);
      }
      ;
      throw new Error("Failed pattern match at Page (line 138, column 36 - line 142, column 55): " + [v.value1.constructor.name]);
    };
    return map30(go2)(state3.toastMessages);
  };
  var handleTerminalOutput = function(dictMonadAff) {
    var monadEffectHalogenM2 = monadEffectHalogenM(dictMonadAff.MonadEffect0());
    var liftEffect13 = liftEffect(monadEffectHalogenM2);
    var parseEchonetLiteFrame1 = parseEchonetLiteFrame2(monadEffectHalogenM2);
    return function(v) {
      if (v instanceof Message) {
        var newToast = new ToastMessage(v.value0);
        return bind18(gets4(function(v1) {
          return v1.toastMessages;
        }))(function(toastMessages) {
          return modify_5(function(v1) {
            var $154 = {};
            for (var $155 in v1) {
              if ({}.hasOwnProperty.call(v1, $155)) {
                $154[$155] = v1[$155];
              }
              ;
            }
            ;
            $154.toastMessages = snoc(toastMessages)(new Tuple(true, newToast));
            return $154;
          });
        });
      }
      ;
      if (v instanceof MessageError) {
        var newToast = new ToastMessageError({
          at: v.value0.at,
          message: message(v.value0.error)
        });
        return bind18(gets4(function(v1) {
          return v1.toastMessages;
        }))(function(toastMessages) {
          return modify_5(function(v1) {
            var $158 = {};
            for (var $159 in v1) {
              if ({}.hasOwnProperty.call(v1, $159)) {
                $158[$159] = v1[$159];
              }
              ;
            }
            ;
            $158.toastMessages = snoc(toastMessages)(new Tuple(true, newToast));
            return $158;
          });
        });
      }
      ;
      if (v instanceof ArraivalResponce) {
        return bind18(gets4(function(v1) {
          return v1.toastMessages;
        }))(function(toastMessages) {
          return bind18(function() {
            if (v.value0.responce instanceof ResEVENT) {
              return pure20([new ToastMessageEvent({
                at: v.value0.at,
                message: toStringEvent(v.value0.responce.value0).message
              })]);
            }
            ;
            if (v.value0.responce instanceof ResEPANDESC) {
              return discard24(modify_5(function(v1) {
                var $166 = {};
                for (var $167 in v1) {
                  if ({}.hasOwnProperty.call(v1, $167)) {
                    $166[$167] = v1[$167];
                  }
                  ;
                }
                ;
                $166.maybeEpandesc = new Just(v.value0.responce.value0);
                return $166;
              }))(function() {
                return pure20([new ToastMessageEvent({
                  at: v.value0.at,
                  message: show10(v.value0.responce.value0)
                })]);
              });
            }
            ;
            if (v.value0.responce instanceof ResERXUDP) {
              return bind18(bind18(liftEffect13(execPut(foldM6(function(v1) {
                return function(x) {
                  return putUint83(x);
                };
              })(mempty5)(v.value0.responce.value0.payload))))(function(echonetliteframe) {
                return parseEchonetLiteFrame1(whole(echonetliteframe));
              }))(function(v1) {
                if (v1 instanceof Left) {
                  return pure20([new ToastMessageEvent({
                    at: v.value0.at,
                    message: show14(v.value0.responce.value0)
                  })]);
                }
                ;
                if (v1 instanceof Right) {
                  var toStringProperty = function(p4) {
                    return map114(toStringWhmProperty)(makeSmartWhmProperty(p4));
                  };
                  var messages = mapMaybe(toStringProperty)(v1.value0.props);
                  return pure20(map30(function(x) {
                    return new ToastMessageEvent({
                      at: v.value0.at,
                      message: x
                    });
                  })(messages));
                }
                ;
                throw new Error("Failed pattern match at Page (line 752, column 15 - line 761, column 85): " + [v1.constructor.name]);
              });
            }
            ;
            if (v.value0.responce instanceof ResIpV6Address) {
              return discard24(modify_5(function(v1) {
                var $174 = {};
                for (var $175 in v1) {
                  if ({}.hasOwnProperty.call(v1, $175)) {
                    $174[$175] = v1[$175];
                  }
                  ;
                }
                ;
                $174.maybeIpV6Address = new Just(v.value0.responce.value0);
                return $174;
              }))(function() {
                return pure20([new ToastMessageEvent({
                  at: v.value0.at,
                  message: show25(v.value0.responce.value0)
                })]);
              });
            }
            ;
            if (v.value0.responce instanceof ResFAIL) {
              return pure20([new ToastMessageError({
                at: v.value0.at,
                message: show33(v.value0.responce.value0)
              })]);
            }
            ;
            if (v.value0.responce instanceof ResOK) {
              return pure20([new ToastMessageEvent({
                at: v.value0.at,
                message: "OK"
              })]);
            }
            ;
            if (v.value0.responce instanceof ResLocalEcho) {
              return pure20([new ToastMessageLocalEcho({
                at: v.value0.at,
                message: v.value0.responce.value0
              })]);
            }
            ;
            if (v.value0.responce instanceof ResMessage) {
              return pure20([new ToastMessageEvent({
                at: v.value0.at,
                message: v.value0.responce.value0
              })]);
            }
            ;
            throw new Error("Failed pattern match at Page (line 734, column 15 - line 776, column 55): " + [v.value0.responce.constructor.name]);
          }())(function(toasts) {
            var messages = map30(function(a2) {
              return new Tuple(true, a2);
            })(toasts);
            return modify_5(function(v1) {
              var $181 = {};
              for (var $182 in v1) {
                if ({}.hasOwnProperty.call(v1, $182)) {
                  $181[$182] = v1[$182];
                }
                ;
              }
              ;
              $181.toastMessages = append14(toastMessages)(messages);
              return $181;
            });
          });
        });
      }
      ;
      if (v instanceof SerialportOpened) {
        return discard24(liftEffect13(logShow3("Opened")))(function() {
          return modify_5(function(v1) {
            var $187 = {};
            for (var $188 in v1) {
              if ({}.hasOwnProperty.call(v1, $188)) {
                $187[$188] = v1[$188];
              }
              ;
            }
            ;
            $187.serialportOpened = true;
            return $187;
          });
        });
      }
      ;
      if (v instanceof SerialportClosed) {
        return discard24(liftEffect13(logShow3("Page.purs:587 Closed")))(function() {
          return modify_5(function(v1) {
            var $190 = {};
            for (var $191 in v1) {
              if ({}.hasOwnProperty.call(v1, $191)) {
                $190[$191] = v1[$191];
              }
              ;
            }
            ;
            $190.serialportOpened = false;
            return $190;
          });
        });
      }
      ;
      throw new Error("Failed pattern match at Page (line 721, column 24 - line 785, column 45): " + [v.constructor.name]);
    };
  };
  var eqToastMessage = {
    eq: function(x) {
      return function(y) {
        if (x instanceof ToastMessage && y instanceof ToastMessage) {
          return eq7(x.value0.at)(y.value0.at) && x.value0.message === y.value0.message;
        }
        ;
        if (x instanceof ToastMessageError && y instanceof ToastMessageError) {
          return eq7(x.value0.at)(y.value0.at) && x.value0.message === y.value0.message;
        }
        ;
        if (x instanceof ToastMessageEvent && y instanceof ToastMessageEvent) {
          return eq7(x.value0.at)(y.value0.at) && x.value0.message === y.value0.message;
        }
        ;
        if (x instanceof ToastMessageLocalEcho && y instanceof ToastMessageLocalEcho) {
          return eq7(x.value0.at)(y.value0.at) && x.value0.message === y.value0.message;
        }
        ;
        return false;
      };
    }
  };
  var dangerAleatDialog = function(s) {
    return div2([classes([m2, alert2, alertDanger, fs5])])([iconExclamationTriangleFill, text5(s)]);
  };
  var _terminal = /* @__PURE__ */ function() {
    return $$Proxy.value;
  }();
  var handleAction3 = function(dictMonadAff) {
    var liftEffect13 = liftEffect(monadEffectHalogenM(dictMonadAff.MonadEffect0()));
    var timer1 = timer(monadAffHalogenM(dictMonadAff));
    var handleTerminalOutput1 = handleTerminalOutput(dictMonadAff);
    return function(v) {
      if (v instanceof Initialize4) {
        return bind18(liftEffect13(available))(function(api) {
          return bind18(request2(_terminal)(unit)(IsOpenedSerialPort.create))(function(maybeSerialPortOpened) {
            return bind18(bindFlipped11(subscribe2)(timer1(Tick.value)))(function(sid) {
              return discard24(modify_5(function(v1) {
                var $204 = {};
                for (var $205 in v1) {
                  if ({}.hasOwnProperty.call(v1, $205)) {
                    $204[$205] = v1[$205];
                  }
                  ;
                }
                ;
                $204.maybeTimer = new Just(sid);
                $204.availableWebSerialApi = api;
                $204.serialportOpened = fromMaybe(false)(maybeSerialPortOpened);
                return $204;
              }))(function() {
                return liftEffect13(bootstrapToastShow);
              });
            });
          });
        });
      }
      ;
      if (v instanceof Finalize4) {
        return mempty12;
      }
      ;
      if (v instanceof Tick) {
        return bind18(liftEffect13(now))(function(instant2) {
          return modify_5(function(v1) {
            var $207 = {};
            for (var $208 in v1) {
              if ({}.hasOwnProperty.call(v1, $208)) {
                $207[$208] = v1[$208];
              }
              ;
            }
            ;
            $207.instant = instant2;
            return $207;
          });
        });
      }
      ;
      if (v instanceof OnClickOpenPortButton) {
        return tell3(_terminal)(unit)(OpenSerialPort.create);
      }
      ;
      if (v instanceof OnClickClosePortButton) {
        return tell3(_terminal)(unit)(CloseSerialPort.create);
      }
      ;
      if (v instanceof OnClickSetCommandLineButton) {
        return tell3(_terminal)(unit)(SetCommandLineString.create(v.value0));
      }
      ;
      if (v instanceof OnClickToastHideButton) {
        var unsetflag = function(dictEq) {
          var eq22 = eq(dictEq);
          return function(value15) {
            return function(xs) {
              return bind19(findIndex(function(v1) {
                return eq22(v1.value1)(value15);
              })(xs))(function(index5) {
                return modifyAt(index5)(function(v1) {
                  return new Tuple(false, v1.value1);
                })(xs);
              });
            };
          };
        };
        var unsetflag1 = unsetflag(eqToastMessage);
        return bind18(gets4(function(v1) {
          return v1.toastMessages;
        }))(function(toastMessages) {
          var v1 = unsetflag1(v.value0)(toastMessages);
          if (v1 instanceof Nothing) {
            return mempty12;
          }
          ;
          if (v1 instanceof Just) {
            return modify_5(function(v2) {
              var $218 = {};
              for (var $219 in v2) {
                if ({}.hasOwnProperty.call(v2, $219)) {
                  $218[$219] = v2[$219];
                }
                ;
              }
              ;
              $218.toastMessages = v1.value0;
              return $218;
            });
          }
          ;
          throw new Error("Failed pattern match at Page (line 714, column 9 - line 716, column 58): " + [v1.constructor.name]);
        });
      }
      ;
      if (v instanceof HandleTerminalUpdate) {
        return handleTerminalOutput1(v.value0);
      }
      ;
      if (v instanceof HandleEditFormUpdate) {
        return tell3(_terminal)(unit)(SendCommand.create(v.value0.value0));
      }
      ;
      throw new Error("Failed pattern match at Page (line 685, column 16 - line 718, column 117): " + [v.constructor.name]);
    };
  };
  var _editform = /* @__PURE__ */ function() {
    return $$Proxy.value;
  }();
  var render3 = function(dictMonadAff) {
    var component1 = component(dictMonadAff);
    var component22 = component2(dictMonadAff);
    return function(state3) {
      var panId = map114(function($229) {
        return function(v) {
          return v.panId;
        }(unwrap8($229));
      })(state3.maybeEpandesc);
      var pairId = map114(function($230) {
        return function(v) {
          return v.pairId;
        }(unwrap8($230));
      })(state3.maybeEpandesc);
      var lqi = map114(function($231) {
        return function(v) {
          return v.lqi;
        }(unwrap8($231));
      })(state3.maybeEpandesc);
      var rssi = map114(makeRSSIfromLQI)(lqi);
      var ipv6address = map114(unwrap8)(state3.maybeIpV6Address);
      var footer2 = div2([style("margin-bottom: 25vh;")])([]);
      var channelPage = map114(function($232) {
        return function(v) {
          return v.channelPage;
        }(unwrap8($232));
      })(state3.maybeEpandesc);
      var channel = map114(function($233) {
        return function(v) {
          return v.channel;
        }(unwrap8($233));
      })(state3.maybeEpandesc);
      var addr = map114(function($234) {
        return function(v) {
          return v.addr;
        }(unwrap8($234));
      })(state3.maybeEpandesc);
      return div_([navigationBar, main([classes([container, pb5])])([div2([style("z-index: 1000"), classes([toastContainer, positionFixed, top0, end0, p3])])(toastDialog(state3)), p([classes([m1])])([a([href4("https://github.com/ak1211/smartmeter-route-b-app")])([text5("\u30B9\u30DE\u30FC\u30C8\u30E1\u30FC\u30BF\u30FC \u30EB\u30FC\u30C8 B \u30C6\u30B9\u30C8\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3")]), text5("\xA9 2023 Akihiro Yamamoto.")]), p([classes([m1])])([text5("\u3053\u306EWeb\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u306F"), a([href4("https://developer.mozilla.org/ja/docs/Web/API/Web_Serial_API")])([text5("Web Serial API")]), text5("\u3092\u4F7F\u7528\u3057\u3066\u3044\u307E\u3059\u3002")]), function() {
        if (state3.availableWebSerialApi) {
          return successAleatDialog("Web Serial API\u304C\u4F7F\u7528\u3067\u304D\u307E\u3059\u3002");
        }
        ;
        if (!state3.availableWebSerialApi) {
          return dangerAleatDialog("Web Serial API\u306B\u5BFE\u5FDC\u3057\u305F\u30D6\u30E9\u30A6\u30B6\u3067\u958B\u3044\u3066\u304F\u3060\u3055\u3044\u3002");
        }
        ;
        throw new Error("Failed pattern match at Page (line 214, column 11 - line 216, column 74): " + [state3.availableWebSerialApi.constructor.name]);
      }(), h2_([text5("\u30CF\u30FC\u30C9\u30A6\u30A8\u30A2\u306E\u6E96\u5099")]), p_([ul_([li_([text5("USB-\u30B7\u30EA\u30A2\u30EB\u5909\u63DB\u5668\u306B\u3064\u306A\u3044\u3060BP35A1")]), li_([text5("USB\u30C9\u30F3\u30B0\u30EB WSR35A1-00")]), li_([text5("USB\u30C9\u30F3\u30B0\u30EB RL7023 Stick-D/IPS")])]), text5("\u306A\u3069\u3092USB\u30DD\u30FC\u30C8\u306B\u633F\u5165\u3059\u308B\u3002"), br_, text5("\u307E\u305F\u306F\u30B3\u30F3\u30D4\u30E5\u30FC\u30BF\u30FC\u306E\u30B7\u30EA\u30A2\u30EB\u30DD\u30FC\u30C8\u306B\u76F4\u63A5BP35A1\u3092\u63A5\u7D9A\u3057\u3066\u304F\u3060\u3055\u3044\u3002")]), p_([text5("\u30B3\u30F3\u30D4\u30E5\u30FC\u30BF\u30FC\u3068BP35A1\u304C\u30B7\u30EA\u30A2\u30EB\u30DD\u30FC\u30C8\u3092\u4ECB\u3057\u3066\u4F1A\u8A71\u3067\u304D\u308B\u3088\u3046\u306B\u63A5\u7D9A\u51FA\u6765\u305F\u3089\u3001"), text5("\u3053\u306E\u30DC\u30BF\u30F3\u3092\u62BC\u3057\u3066\u30B7\u30EA\u30A2\u30EB\u30DD\u30FC\u30C8\u3092\u958B\u304D\u307E\u3059\u3002")]), div2([class_(m3)])([button([classes(concat([[m1, btn], function() {
        if (state3.serialportOpened) {
          return [disabled10, btnSecondary];
        }
        ;
        return [btnPrimary];
      }()])), onClick(function(v) {
        return OnClickOpenPortButton.value;
      })])([text5("\u30B7\u30EA\u30A2\u30EB\u30DD\u30FC\u30C8\u3092\u958B\u304F")]), button([classes(concat([[m1, btn], function() {
        if (state3.serialportOpened) {
          return [btnPrimary];
        }
        ;
        return [disabled10, btnSecondary];
      }()])), onClick(function(v) {
        return OnClickClosePortButton.value;
      })])([text5("\u30B7\u30EA\u30A2\u30EB\u30DD\u30FC\u30C8\u3092\u9589\u3058\u308B")])]), h2_([text5("BP35A1\u3068\u4F1A\u8A71\u3057\u3066\u307F\u308B")]), p_([text5("\u30BF\u30FC\u30DF\u30CA\u30EB\u306BPort opened\u304C\u8868\u793A\u3055\u308C\u3066\u3044\u308B\u306E\u3092\u78BA\u8A8D\u3057\u305F\u3089\u3001\u758E\u901A\u78BA\u8A8D\u306E\u305F\u3081\u306B\u3068\u308A\u3042\u3048\u305ASKVER\u3068\u5165\u529B\u3057\u3066\u9001\u4FE1\u3059\u308B\u3002")]), button([classes([btn, btnSm, btnOutlinePrimary]), onClick(function(v) {
        return new OnClickSetCommandLineButton("SKVER");
      })])([text5("SKVER")]), p_([text5("SKVER\u3068\u5165\u529B\u3059\u308B\u304B\u3001\u307E\u305F\u306F\u3053\u306E\u30DC\u30BF\u30F3\u3092\u62BC\u3059\u3068SKVER\u304C\u5165\u529B\u3055\u308C\u308B\u306E\u3067\u3001\u9001\u4FE1\u3092\u62BC\u3059\u3053\u3068\u3067BP35A1\u307E\u305F\u306FRL7023\u306BSKVER\u3092\u9001\u4FE1\u3059\u308B"), br_, text5("\u3053\u306E\u5F8C"), br_, text5("EVER 1.2.8"), br_, text5("OK"), br_, text5("\u306A\u3069\u306E\u5FDC\u7B54\u304B\u3042\u308B\u3068BP35A1\u3068\u306E\u901A\u4FE1\u304C\u3067\u304D\u3066\u3044\u308B\u3002"), text5("\u305D\u3046\u3067\u306A\u3051\u308C\u3070\u63A5\u7D9A\u3092\u898B\u76F4\u3057\u3066\u304F\u3060\u3055\u3044\u3002")]), h5_([text5("\u8868\u793A\u5F62\u5F0F\u3092\u898B\u3066\u307F\u308B")]), button([classes([btn, btnSm, btnOutlinePrimary]), onClick(function(v) {
        return new OnClickSetCommandLineButton("ROPT");
      })])([text5("ROPT")]), p_([text5("ROPT\u306F\u30C7\u30FC\u30BF\u90E8\u306E\u8868\u793A\u5F62\u5F0F\u3092\u8AAD\u307F\u3060\u3059\u3002"), text5("OK 00\u307E\u305F\u306FOK 01"), br_, text5("\u3053\u306E\u610F\u5473\u306F"), br_, text5("0: \u30D0\u30A4\u30CA\u30EA\u8868\u793A"), br_, text5("1: 16\u9032ASCII\u8868\u793A"), br_, text5("RL7023 Stick-D/IPS\u3067\u306F16\u9032ASCII\u306E\u307F\u30B5\u30DD\u30FC\u30C8\u306A\u306E\u3067ER04\u306B\u306A\u308B\u3002"), br_, text5("\u3053\u306E\u30D7\u30ED\u30B0\u30E9\u30E0\u306F16\u9032ASCII\u8868\u793A\u3067\u3042\u308B\u3053\u3068\u304C\u524D\u63D0\u3067\u3059\u3002"), br_, text5("\u30D0\u30A4\u30CA\u30EA\u8868\u793A\u306A\u3089WOPT 01\u3092\u9001\u4FE1\u3057\u306616\u9032ASCII\u8868\u793A\u306B\u5207\u308A\u66FF\u3048\u3066\u304F\u3060\u3055\u3044"), br_]), h5_([text5("\u4ED6\u306E\u30EC\u30B8\u30B9\u30BF\u306E\u5185\u5BB9\u3082\u898B\u3066\u307F\u308B")]), button([classes([btn, btnSm, btnOutlinePrimary]), onClick(function(v) {
        return new OnClickSetCommandLineButton("SKSREG SFE");
      })])([text5("SKSREG SFE")]), p_([text5("SKSREG SFE\u306F\u30A8\u30B3\u30FC\u30D0\u30C3\u30AF\u30D5\u30E9\u30B0\u3092\u8AAD\u307F\u3060\u3059\u3002"), br_, text5("ESREG 1"), br_, text5("OK"), br_, text5("\u3053\u306E\u610F\u5473\u306F"), br_, text5("0: \u30B3\u30DE\u30F3\u30C9\u5165\u529B\u306E\u30A8\u30B3\u30FC\u30D0\u30C3\u30AF\u3092\u3057\u306A\u3044"), br_, text5("1: \u30A8\u30B3\u30FC\u30D0\u30C3\u30AF\u3042\u308A"), br_, text5("\u3067\u3059\u3002")]), h2_([text5("\u30B9\u30DE\u30FC\u30C8\u30E1\u30FC\u30BF\u30FC\u3068\u63A5\u7D9A\u3059\u308B")]), p_([text5("\u9001\u914D\u96FB\u4E8B\u696D\u8005\u304B\u3089\u3082\u3089\u3063\u305F\u8A8D\u8A3CID\u3068\u30D1\u30B9\u30EF\u30FC\u30C9\u3092\u7528\u610F\u3059\u308B\u3002")]), h5_([text5("\u30D1\u30B9\u30EF\u30FC\u30C9\u3092\u767B\u9332\u3059\u308B\u3002")]), p_([button([classes([btn, btnSm, btnOutlinePrimary]), onClick(function(v) {
        return new OnClickSetCommandLineButton("SKSETPWD C ");
      })])([text5("SKSETPWD C ")]), br_, text5("SKSETPWD C \u306B\u3064\u3065\u3051\u3066\u30D1\u30B9\u30EF\u30FC\u30C9\u3092\u5165\u529B\u3057\u3066\u9001\u4FE1\u3059\u308B\u3002")]), h5_([text5("\u8A8D\u8A3CID\u3092\u767B\u9332\u3059\u308B\u3002")]), p_([button([classes([btn, btnSm, btnOutlinePrimary]), onClick(function(v) {
        return new OnClickSetCommandLineButton("SKSETRBID ");
      })])([text5("SKSETRBID ")]), br_, text5("SKSETRBID \u306B\u3064\u3065\u3051\u3066\u8A8D\u8A3CID\u3092\u5165\u529B\u3057\u3066\u9001\u4FE1\u3059\u308B\u3002")]), h2_([text5("\u30A2\u30AF\u30C6\u30A3\u30D6\u30B9\u30AD\u30E3\u30F3\u3092\u5B9F\u884C\u3059\u308B\u3002")]), p_([text5("SKSCAN 2 \u3092\u9001\u4FE1\u3057\u305F\u5F8C\u306FEVENT 0x22 (\u30A2\u30AF\u30C6\u30A3\u30D6\u30B9\u30AD\u30E3\u30F3\u306E\u5B8C\u4E86)\u306E\u5FDC\u7B54\u3092\u5F85\u3064\u3053\u3068\u3002"), br_, text5("EPANDESC\u306E\u30A4\u30D9\u30F3\u30C8\u304C\u3053\u306A\u3051\u308C\u3070\u7D9A\u3051\u3066SKSCAN 2\u3092\u9001\u4FE1\u3059\u308B(duration\u3092\u5927\u304D\u304F\u3057\u3066)"), br_, text5("EVENT 0x20 (Beacon\u3092\u53D7\u4FE1\u3057\u305F)\u306E\u5F8C\u306BEPANDESC\u304C\u3084\u3063\u3066\u304F\u308B\u307E\u3067duration\u3092\u5927\u304D\u304F\u3057\u306A\u304C\u3089\u7E70\u308A\u8FD4\u3059\u3002"), br_, text5("\u30DC\u30BF\u30F3\u3092\u304A\u3044\u3066\u304A\u304F\u306E\u3067\u3001\u8A66\u3057\u3066\u304F\u3060\u3055\u3044\u3002")]), h5_([text5("duration: 4 \u3067\u30A2\u30AF\u30C6\u30A3\u30D6\u30B9\u30AD\u30E3\u30F3\u3092\u5B9F\u884C\u3059\u308B\u3002")]), p_([button([classes([btn, btnSm, btnOutlinePrimary]), onClick(function(v) {
        return new OnClickSetCommandLineButton("SKSCAN 2 FFFFFFFF 4");
      })])([text5("SKSCAN 2 FFFFFFFF 4")])]), h5_([text5("duration: 5 \u3067\u30A2\u30AF\u30C6\u30A3\u30D6\u30B9\u30AD\u30E3\u30F3\u3092\u5B9F\u884C\u3059\u308B\u3002")]), p_([button([classes([btn, btnSm, btnOutlinePrimary]), onClick(function(v) {
        return new OnClickSetCommandLineButton("SKSCAN 2 FFFFFFFF 5");
      })])([text5("SKSCAN 2 FFFFFFFF 5")])]), h5_([text5("duration: 6 \u3067\u30A2\u30AF\u30C6\u30A3\u30D6\u30B9\u30AD\u30E3\u30F3\u3092\u5B9F\u884C\u3059\u308B\u3002")]), p_([button([classes([btn, btnSm, btnOutlinePrimary]), onClick(function(v) {
        return new OnClickSetCommandLineButton("SKSCAN 2 FFFFFFFF 6");
      })])([text5("SKSCAN 2 FFFFFFFF 6")])]), h5_([text5("duration: 7 \u3067\u30A2\u30AF\u30C6\u30A3\u30D6\u30B9\u30AD\u30E3\u30F3\u3092\u5B9F\u884C\u3059\u308B\u3002")]), p_([button([classes([btn, btnSm, btnOutlinePrimary]), onClick(function(v) {
        return new OnClickSetCommandLineButton("SKSCAN 2 FFFFFFFF 7");
      })])([text5("SKSCAN 2 FFFFFFFF 7")])]), h5_([text5("duration: 8 \u3067\u30A2\u30AF\u30C6\u30A3\u30D6\u30B9\u30AD\u30E3\u30F3\u3092\u5B9F\u884C\u3059\u308B\u3002")]), p_([button([classes([btn, btnSm, btnOutlinePrimary]), onClick(function(v) {
        return new OnClickSetCommandLineButton("SKSCAN 2 FFFFFFFF 8");
      })])([text5("SKSCAN 2 FFFFFFFF 8")])]), h2_([text5("EPANDESC\u30A4\u30D9\u30F3\u30C8\u304B\u3089\u5FC5\u8981\u306A\u60C5\u5831\u3092\u53D6\u308A\u51FA\u3059\u3002")]), p_([text5("\u3053\u3093\u306A\u5FDC\u7B54\u304C\u6765\u308B\u306F\u305A\u3002(EPANDESC\u30A4\u30D9\u30F3\u30C8\u3002\u4E00\u90E8\u96A0\u3057\u3066\u3044\u307E\u3059)"), br_, pre_([text5("EVENT 20 0000:0000:0000:0000:0000:0000:0000:0000\r\n"), text5("EPANDESC\r\n"), text5("  Channel:3B\r\n"), text5("  Channel Page:09\r\n"), text5("  Pan ID:0000\r\n"), text5("  Addr:0000000000000000\r\n"), text5("  LQI:35\r\n"), text5("  PairID:00000000\r\n")])]), p_([text5("\u3046\u307E\u304F\u3044\u3063\u3066\u3044\u308B\u3068\u3001\u4EE5\u4E0B\u306B\u8868\u793A\u3055\u308C\u308B")]), div2([classes([alert2, alertPrimary])])([table_(function() {
        var c = [classes([p2])];
        return [thead_([]), tbody_([tr_([td(c)([text5("Channel")]), td(c)([text5(fromMaybe("\u4E0D\u660E")(channel))])]), tr_([td(c)([text5("Channel Page")]), td(c)([text5(fromMaybe("\u4E0D\u660E")(channelPage))])]), tr_([td(c)([text5("Pan ID")]), td(c)([text5(fromMaybe("\u4E0D\u660E")(panId))])]), tr_([td(c)([text5("Addr")]), td(c)([text5(fromMaybe("\u4E0D\u660E")(map114(show42)(addr)))])]), tr_([td(c)([text5("LQI(10\u9032\u5024)")]), td(c)([text5(maybe("\u4E0D\u660E")(toString)(lqi))])]), tr_([td(c)([text5("LQI(RSSI)")]), td(c)([text5(maybe("\u4E0D\u660E")(function($235) {
          return toString2(unwrap8($235));
        })(rssi))]), td(c)([text5("dBm")])]), tr_([td(c)([text5("PairID:")]), td(c)([text5(fromMaybe("\u4E0D\u660E")(pairId))])])])];
      }())]), h5_([text5("MAC\u30A2\u30C9\u30EC\u30B9\u304B\u3089IPv6\u30EA\u30F3\u30AF\u30ED\u30FC\u30AB\u30EB\u30A2\u30C9\u30EC\u30B9\u3078\u5909\u63DB\u3059\u308B")]), p_(function() {
        var text6 = "SKLL64 " + maybe("")(unwrap8)(addr);
        return [button([classes([btn, btnSm, btnOutlinePrimary]), onClick(function(v) {
          return new OnClickSetCommandLineButton(text6);
        })])([text5(text6)]), br_, text5("SKLL64 \u306B\u3064\u3065\u3051\u3066EPANDESC\u3067\u6307\u5B9A\u3055\u308C\u3066\u3044\u308BAddr\u3092\u5165\u529B\u3057\u3066\u9001\u4FE1\u3059\u308B\u3002"), br_, text5("FE80:0000:0000:0000:0000:0000:0000:0000"), br_, text5("\u3053\u3093\u306A\u304B\u3093\u3058\u306E\u5FDC\u7B54\u304C\u3042\u308B\u3002\u3053\u308C\u304CIPv6\u30EA\u30F3\u30AF\u30ED\u30FC\u30AB\u30EB\u30A2\u30C9\u30EC\u30B9\u3002")];
      }()), p_([text5("\u3046\u307E\u304F\u3044\u3063\u3066\u3044\u308B\u3068\u3001\u4EE5\u4E0B\u306B\u8868\u793A\u3055\u308C\u308B")]), div2([classes([alert2, alertPrimary])])([table_(function() {
        var c = [classes([p2])];
        return [thead_([]), tbody_([tr_([td(c)([text5("IPV6\u30A2\u30C9\u30EC\u30B9")]), td(c)([text5(fromMaybe("\u4E0D\u660E")(ipv6address))])])])];
      }())]), h5_([text5("MAC\u30A2\u30C9\u30EC\u30B9\u304B\u3089IPv6\u30EA\u30F3\u30AF\u30ED\u30FC\u30AB\u30EB\u30A2\u30C9\u30EC\u30B9\u3078\u5909\u63DB\u3059\u308B")]), p_(function() {
        var text6 = "SKSREG S2 " + fromMaybe("")(channel);
        return [text5("\u81EA\u7AEF\u672B\u304C\u4F7F\u7528\u3059\u308B\u5468\u6CE2\u6570\u306E\u8AD6\u7406\u30C1\u30E3\u30F3\u30CD\u30EB\u756A\u53F7\u3092\u8A2D\u5B9A\u3059\u308B"), br_, button([classes([btn, btnSm, btnOutlinePrimary]), onClick(function(v) {
          return new OnClickSetCommandLineButton(text6);
        })])([text5(text6)]), br_, text5("SKSREG S2 \u306B\u3064\u3065\u3051\u3066EPANDESC\u3067\u6307\u5B9A\u3055\u308C\u3066\u3044\u308BChannel\u3092\u5165\u529B\u3057\u3066\u9001\u4FE1\u3059\u308B\u3002")];
      }()), h5_([text5("\u81EA\u7AEF\u672B\u306EPAN ID\u3092\u8A2D\u5B9A\u3059\u308B")]), p_(function() {
        var text6 = "SKSREG S3 " + fromMaybe("")(panId);
        return [button([classes([btn, btnSm, btnOutlinePrimary]), onClick(function(v) {
          return new OnClickSetCommandLineButton(text6);
        })])([text5(text6)]), br_, text5("SKSREG S3 \u306B\u3064\u3065\u3051\u3066EPANDESC\u3067\u6307\u5B9A\u3055\u308C\u3066\u3044\u308BPan ID\u3092\u5165\u529B\u3057\u3066\u9001\u4FE1\u3059\u308B\u3002")];
      }()), h5_([text5("PANA\u8A8D\u8A3C")]), p_(function() {
        var text6 = "SKJOIN " + fromMaybe("")(ipv6address);
        return [button([classes([btn, btnSm, btnOutlinePrimary]), onClick(function(v) {
          return new OnClickSetCommandLineButton(text6);
        })])([text5(text6)]), br_, text5("SKJOIN \u306B\u3064\u3065\u3051\u3066IPv6\u30EA\u30F3\u30AF\u30ED\u30FC\u30AB\u30EB\u30A2\u30C9\u30EC\u30B9\u3092\u5165\u529B\u3057\u3066\u9001\u4FE1\u3059\u308B\u3002")];
      }()), p_([text5("\u6210\u529F\u3059\u308B\u3068\u30A4\u30D9\u30F3\u30C8\u304C\u305F\u304F\u3055\u3093\u9001\u3089\u308C\u3066\u304F\u308B\u3002"), br_, text5("\u305D\u306E\u4E2D\u306BEVENT25\u304C\u3042\u308B\u3068\u63A5\u7D9A\u5B8C\u4E86\u3002")]), p([classes([m5, p5])])([strong_([text5("EVENT 25\u53D7\u4FE1\u6642\u70B9\u3067\u30B9\u30DE\u30FC\u30C8\u30E1\u30FC\u30BF\u30FC\u3068\u306E\u30BB\u30C3\u30B7\u30E7\u30F3\u304C\u78BA\u7ACB\u3059\u308B\u3002")])]), h2_([text5("ERXUDP\u30A4\u30D9\u30F3\u30C8\u3092\u8AAD\u3093\u3067\u307F\u308B")]), p_([text5("ERXUDP\u30A4\u30D9\u30F3\u30C8\u306E\u69CB\u9020\u306F"), ul_([li_([text5("ERXUDP")]), li_([text5("\u9001\u4FE1\u5143IPv6\u30A2\u30C9\u30EC\u30B9")]), li_([text5("\u9001\u4FE1\u5148IPv6\u30A2\u30C9\u30EC\u30B9")]), li_([text5("\u9001\u4FE1\u5143\u30DD\u30FC\u30C8\u756A\u53F7")]), li_([text5("\u9001\u4FE1\u5148\u30DD\u30FC\u30C8\u756A\u53F7")]), li_([text5("\u9001\u4FE1\u5143\u306EMAC\u5C64\u30A2\u30C9\u30EC\u30B9")]), li_([text5("1")]), li_([text5("\u53D7\u4FE1\u3057\u305F\u30C7\u30FC\u30BF\u306E\u9577\u3055(16\u9032\u6570)")]), li_([text5("\u53D7\u4FE1\u30C7\u30FC\u30BF")])])]), p_([text5("\u63A5\u7D9A\u5B8C\u4E86\u6642\u306B\u9001\u3089\u308C\u3066\u304D\u305FERXUDP\u30A4\u30D9\u30F3\u30C8\u3092\u8AAD\u3093\u3067\u307F\u308B")]), pre_([text5("ERXUDP FE80:0000:0000:0000:0000:0000:0000:0000 FF02:0000:0000:0000:0000:0000:0000:0000 0E1A 0E1A 0000000000000000 1 0012 10812EDA0EF0010EF0017301D50401028801")]), p_([text5("IPv6\u30A2\u30C9\u30EC\u30B9\u306F\u914D\u9001\u7528\u306E\u60C5\u5831, Echonet Lite\u306E\u30DD\u30FC\u30C8\u306F 0E1A \u56FA\u5B9A\u5024"), text5("\u306A\u306E\u3067\u8AAD\u3093\u3067\u610F\u5473\u304C\u3042\u308B\u60C5\u5831\u306F\u6700\u5F8C\u306E\u30DA\u30A4\u30ED\u30FC\u30C9\u90E8\u5206\u3002"), br_, text5("\u305D\u308C\u3092\u53D6\u308A\u51FA\u3059\u3068"), br_, text5("0012 10812EDA0EF0010EF0017301D50401028801"), br_, text5("0012 (16\u9032\u6570) = 18\u30D0\u30A4\u30C8, 1\u30D0\u30A4\u30C8\u306F16\u9032\u65702\u6587\u5B57\u3060\u304B\u30892\u6587\u5B57*18=\u30C7\u30FC\u30BF\u306F16\u9032\u6570\u306736\u6587\u5B57"), br_, text5("Echonet Lite\u96FB\u6587\u69CB\u6210(\u30D5\u30EC\u30FC\u30E0\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8)\u306B\u3088\u308B\u3068"), ul_([li_([text5("EHD1 (1byte)")]), li_([text5("EHD2 (1byte)")]), li_([text5("TID (2bytes)")]), li_([text5("\u3053\u3053\u304B\u3089EDATA"), p_([text5("SEOJ: \u9001\u4FE1\u5143Echonet Lite\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\u6307\u5B9A (3bytes)"), br_, text5("DEOJ: \u9001\u4FE1\u5148Echonet Lite\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\u6307\u5B9A (3bytes)"), br_, text5("ESV: Echonet Lite\u30B5\u30FC\u30D3\u30B9 (1byte)"), br_, text5("OPC: \u51E6\u7406\u30D7\u30ED\u30D1\u30C6\u30A3\u6570 (1byte)"), br_, text5("EPC: Echonet Lite\u30D7\u30ED\u30D1\u30C6\u30A3 (1byte)"), br_, text5("PDC: EDT\u306E\u30D0\u30A4\u30C8\u6570 (1byte)"), br_, text5("EDT: \u30D7\u30ED\u30D1\u30C6\u30A3\u5024\u30C7\u30FC\u30BF (PDC\u3067\u6307\u5B9Abytes)")])])]), br_, text5("\u306A\u306E\u3067\u3001\u3053\u308C\u306B\u5F53\u3066\u306F\u3081\u3066\u307F\u308B\u3068"), ul_([li_([text5("0x10 (EHD1)")]), li_([text5("0x81 (EHD2)")]), li_([text5("0x2E 0xDA (TID)")]), li_([text5("\u3053\u3053\u304B\u3089EDATA"), ul_([li_([text5("0x0E 0xF0 0x01 (SEOJ) -> \u30CE\u30FC\u30C9\u30D7\u30ED\u30D5\u30A1\u30A4\u30EB\u30AF\u30E9\u30B9\u306E\u4E00\u822C\u30CE\u30FC\u30C9\u30A4\u30F3\u30B9\u30BF\u30F3\u30B9")]), li_([text5("0x0E 0xF0 0x01 (DEOJ) -> \u30CE\u30FC\u30C9\u30D7\u30ED\u30D5\u30A1\u30A4\u30EB\u30AF\u30E9\u30B9\u306E\u4E00\u822C\u30CE\u30FC\u30C9\u30A4\u30F3\u30B9\u30BF\u30F3\u30B9")]), li_([text5("0x73 (ESV) -> \u30D7\u30ED\u30D1\u30C6\u30A3\u5024\u901A\u77E5")]), li_([text5("0x01 (OPC)")]), li_([text5("0xD5 (EPC) -> \u30A4\u30F3\u30B9\u30BF\u30F3\u30B9\u30EA\u30B9\u30C8\u901A\u77E5")]), li_([text5("0x04 (PDC)")]), li_([text5("0x01 0x02 0x88 0x01 (EDT) -> 1\u3064\u306E028801(\u4F4E\u5727\u30B9\u30DE\u30FC\u30C8\u96FB\u529B\u91CF\u30E1\u30FC\u30BF\u30AF\u30E9\u30B9\u306E\u30A4\u30F3\u30B9\u30BF\u30F3\u30B9)\u304C\u5B58\u5728\u3059\u308B")])])]), text5("\u3068\u3044\u3046\u3053\u3068\u3067\u3001\u3053\u308C\u306FEchonet Lite\u30A4\u30F3\u30B9\u30BF\u30F3\u30B9\u30EA\u30B9\u30C8\u901A\u77E5"), br_])]), h2_([text5("\u30B9\u30DE\u30FC\u30C8\u30E1\u30FC\u30BF\u30FC\u3068\u901A\u4FE1\u3059\u308B")]), p_([text5("\u4E0B\u306E\u30D5\u30A9\u30FC\u30E0\u3067\u30B9\u30DE\u30FC\u30C8\u30E1\u30FC\u30BF\u30FC\u306B\u9001\u4FE1\u3059\u308B\u3068\u3001\u8FD4\u7B54\u304C\u30A4\u30D9\u30F3\u30C8\u3067\u3084\u3063\u3066\u304D\u307E\u3059\u3002"), br_, text5("\u30B9\u30DE\u30FC\u30C8\u30E1\u30FC\u30BF\u30FC\u3068\u306E\u30BB\u30C3\u30B7\u30E7\u30F3\u304C\u78BA\u7ACB\u3057\u3066\u3044\u308B\u3068\u3001\u3068\u304F\u306B\u4F55\u3082\u3057\u306A\u304F\u3066\u3082\u6BCE\u66420\u5206,30\u5206\u306B\u5B9A\u6642\u7A4D\u7B97\u96FB\u529B\u91CF\u30A4\u30D9\u30F3\u30C8\u304C\u3084\u3063\u3066\u304D\u307E\u3059\u3002")]), p_([text5("\u3044\u308D\u3044\u308D\u8A66\u3057\u3066\u307F\u3066\u304F\u3060\u3055\u3044\u3002")]), slot1(_editform)(unit)(component1)({
        maybeIpV6Address: state3.maybeIpV6Address
      })(HandleEditFormUpdate.create), h2_([text5("\u304A\u308F\u308A")]), p_([button([classes([btn, btnSm, btnOutlinePrimary]), onClick(function(v) {
        return new OnClickSetCommandLineButton("SKTERM");
      })])([text5("SKTERM")]), br_, text5("\u6700\u5F8C\u306FSKTERM\u3092\u9001\u4FE1\u3059\u308B\u3053\u3068\u3067\u30B9\u30DE\u30FC\u30C8\u30E1\u30FC\u30BF\u30FC\u3068\u306E\u30BB\u30C3\u30B7\u30E7\u30F3\u3092\u5207\u308A\u307E\u3059\u3002")]), div2([id2("accordionExample"), class_(fixedBottom)])([div2([classes([accordion, fixedBottom])])([div2([class_(accordionItem)])([div2([class_(accordionHeader)])([button([type_21(ButtonButton.value), class_(accordionButton), attr2("data-bs-toggle")("collapse"), attr2("data-bs-target")("#collapseOne"), attr2("aria-expanded")("true"), attr2("aria-controls")("collapseOne")])([text5("\u30BF\u30FC\u30DF\u30CA\u30EB")])]), div2([id2("collapseOne"), classes([accordionCollapse, collapse, show7]), attr2("aria-labelledby")("headingOne"), attr2("data-bs-parent")("#accordionExample")])([div2([class_(accordionBody)])([slot22(_terminal)(unit)(component22)(unit)(HandleTerminalUpdate.create)])])])])])]), footer2]);
    };
  };
  var component3 = function(dictMonadAff) {
    return mkComponent({
      initialState: initialState3,
      render: render3(dictMonadAff),
      "eval": mkEval({
        handleAction: handleAction3(dictMonadAff),
        handleQuery: defaultEval.handleQuery,
        receive: defaultEval.receive,
        initialize: new Just(Initialize4.value),
        finalize: new Just(Finalize4.value)
      })
    });
  };

  // output/Main/index.js
  var component4 = /* @__PURE__ */ component3(monadAffAff);
  var main2 = /* @__PURE__ */ runHalogenAff(/* @__PURE__ */ bind(bindAff)(awaitBody)(function(body2) {
    return runUI2(component4)(unit)(body2);
  }));

  // <stdin>
  main2();
})();
