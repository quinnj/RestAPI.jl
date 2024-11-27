module RestAPI

using HTTP, JSONBase

const ROUTER = HTTP.Router()

# json_middleware
struct Arg
    ispath::Bool
    isquery::Bool # based on if a default value is provided
    name::String
    type::Type
    default::Any
end

function parseargs(path, fexpr)
    @assert fexpr.head == :function "Only functions can be annotated with @GET, @POST, @PUT, or @DELETE"
    pathparams = [x.captures[1] for x in eachmatch(r"\{(\w+)\}", path)]
    sig = fexpr.args[1]
    argexprs = @view sig.args[2:end]
    args = Vector{Expr}(undef, length(argexprs))
    for i = 1:length(argexprs)
        argexpr = argexprs[i]
        if argexpr isa Symbol
            # type-less argument as path param or body
            nm = String(argexpr)
            if nm in pathparams
                args[i] = :(RestAPI.Arg(true, false, $nm, Any, nothing))
            else
                args[i] = :(RestAPI.Arg(false, false, $nm, Any, nothing))
            end
        elseif argexpr.head == :parameters && argexpr.args[1].head == :kw
            # keyword argument as query param
            argex, default = argexpr.args[1].args
            # remove default value from parent expr since we'll apply it ourselves
            argexprs[i] = argex
            if argex isa Symbol
                nm = String(argex)
                args[i] = :(RestAPI.Arg(false, true, $nm, Any, $default))
            else
                nm = String(argex.args[1])
                args[i] = :(RestAPI.Arg(false, true, $nm, $(argex.args[2]), $default))
            end
        elseif argexpr.head == :kw
            # keyword argument as query param
            argex, default = argexpr.args
            # remove default value from parent expr since we'll apply it ourselves
            argexprs[i] = argex
            if argex isa Symbol
                nm = String(argex)
                args[i] = :(RestAPI.Arg(false, true, $nm, Any, $default))
            else
                nm = String(argex.args[1])
                args[i] = :(RestAPI.Arg(false, true, $nm, $(argex.args[2]), $default))
            end
        else
            # type-annotated argument as path param or body
            nm = String(argexpr.args[1])
            if nm in pathparams
                args[i] = :(RestAPI.Arg(true, false, $nm, $(argexpr.args[2]), nothing))
            else
                args[i] = :(RestAPI.Arg(false, false, $nm, $(argexpr.args[2]), nothing))
            end
        end
    end
    return args
end

_string(x::Union{AbstractString, AbstractVector{UInt8}}) = String(x)
_string(x::AbstractString) = string(x)

function infer(arg::Arg, val, quiet::Bool = false)
    try
        if arg.type == Int
            return parse(Int, val)
        elseif arg.type == Float64
            return parse(Float64, val)
        elseif arg.type == Bool
            return val in ("t", "T", "1", "true") ? true :
                   val in ("f", "F", "0", "false") ? false : parse(Bool, val)
        elseif arg.type == String
            return _string(val)
        elseif arg.type == Vector{Int}
            return map(x -> parse(Int, x), split(val, ","))
        elseif arg.type == Vector{Float64}
            return map(x -> parse(Float64, x), split(val, ","))
        elseif arg.type == Vector{Bool}
            return map(x -> parse(Bool, x), split(val, ","))
        elseif arg.type == Vector{String}
            return map(_string, split(val, ","))
        else
            # try to infer val, first Bool, then Int, then Float64, otherwise String
            if val == "true"
                return true
            elseif val == "false"
                return false
            elseif match(r"^[-+]?\d+$", val) !== nothing
                return parse(Int, val)
            elseif match(r"^[-+]?(\d+(\.\d*)?|\.\d+)([eE][-+]?\d+)?$", val) !== nothing
                return parse(Float64, val)
            else
                return _string(val)
            end
        end
    catch e
        quiet || @error "failed to infer/cast argument to type: $(arg.type) from value: $val" exception=(e, catch_backtrace())
        return val
    end
end

function extractargs(fargs::Vector{Arg}, args, req::HTTP.Request)
    resize!(args, length(fargs))
    for (i, arg) in enumerate(fargs)
        if arg.ispath
            args[i] = infer(arg, HTTP.getparams(req)[arg.name])
        elseif arg.isquery
            # check if arg is in query params
            u = HTTP.URI(req.target)
            queryparams = HTTP.URIs.queryparampairs(u)
            found = false
            for (k, v) in queryparams
                if k == arg.name
                    args[i] = infer(arg, v)
                    found = true
                    break
                end
            end
            # if not found, use default value
            found || (args[i] = arg.default)
        else
            # otherwise, materialize from request body
            args[i] = JSONBase.materialize(req.body, arg.type)
        end
    end
    return args
end

function json_middleware(handler, fargs::Vector{Arg})
    args = []
    return function(req::HTTP.Request)
        ret = handler(extractargs(fargs, args, req)...)
        empty!(args)
        return HTTP.Response(200, JSONBase.json(ret))
    end
end

macro GET(router, path, handler)
    fargs = parseargs(path, handler)
    esc(quote
        @info "registering GET route: $($path)"
        RestAPI.HTTP.register!($router, "GET", $path, RestAPI.json_middleware($handler, RestAPI.Arg[$(fargs...)]))
        #TODO: how do we avoid over-writing OPTIONS paths too much?
        # maybe this needs to be builtin to HTTP.Router more tightly?
        RestAPI.HTTP.register!($router, "OPTIONS", $path, req -> RestAPI.HTTP.Response(200))
    end)
end

macro GET(path, handler)
    esc(:(RestAPI.@GET(RestAPI.ROUTER, $path, $handler)))
end

macro POST(router, path, handler)
    fargs = parseargs(path, handler)
    esc(quote
        @info "registering POST route: $($path)"
        RestAPI.HTTP.register!($router, "POST", $path, RestAPI.json_middleware($handler, RestAPI.Arg[$(fargs...)]))
        RestAPI.HTTP.register!($router, "OPTIONS", $path, req -> RestAPI.HTTP.Response(200))
    end)
end

macro POST(path, handler)
    esc(:(RestAPI.@POST(RestAPI.ROUTER, $path, $handler)))
end

macro PUT(router, path, handler)
    fargs = parseargs(path, handler)
    esc(quote
        @info "registering PUT route: $($path)"
        RestAPI.HTTP.register!($router, "PUT", $path, RestAPI.json_middleware($handler, RestAPI.Arg[$(fargs...)]))
        RestAPI.HTTP.register!($router, "OPTIONS", $path, req -> RestAPI.HTTP.Response(200))
    end)
end

macro PUT(path, handler)
    esc(:(RestAPI.@PUT(RestAPI.ROUTER, $path, $handler)))
end

macro DELETE(router, path, handler)
    fargs = parseargs(path, handler)
    esc(quote
        @info "registering DELETE route: $($path)"
        RestAPI.HTTP.register!($router, "DELETE", $path, RestAPI.json_middleware($handler, RestAPI.Arg[$(fargs...)]))
        RestAPI.HTTP.register!($router, "OPTIONS", $path, req -> RestAPI.HTTP.Response(200))
    end)
end

macro DELETE(path, handler)
    esc(:(RestAPI.@DELETE(RestAPI.ROUTER, $path, $handler)))
end

end # module RestAPI
